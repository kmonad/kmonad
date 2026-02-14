{-|
Module      : KMonad.Model.Hooks
Description : Component for handling hooks
Copyright   : (c) David Janssen, 2019
License     : MIT
Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : portable

Part of the KMonad deferred-decision mechanics are implemented using hooks,
which will call predicates and actions on future keypresses and/or timer events.
The 'Hooks' component is the concrete implementation of this functionality.

In the sequencing of components, this happens second, right after the
'KMonad.App.Dispatch.Dispatch' component.

-}
module KMonad.Model.Hooks
  ( Hooks
  , mkHooks
  , pull
  , register
  )
where

import KMonad.Prelude

import Data.Time.Clock.System
import Data.Unique

import KMonad.Model.Action hiding (register)
import KMonad.Model.EventSrc
import KMonad.Keyboard
import KMonad.Util

import RIO.Partial (fromJust)

import qualified RIO.HashMap as M

--------------------------------------------------------------------------------
-- $hooks



-- -- | A 'Hook' contains the 'KeyPred' and 'Callback'
-- newtype Hook = Hook (KeyPred, Callback IO)
-- makeWrapped ''Hook

-- -- | Create a new 'Hook' value
-- mkHook :: MonadUnliftIO m => KeyPred -> Callback m -> m Hook
-- mkHook p c = withRunInIO $ \u -> pure $ Hook (p, (u . c))

--------------------------------------------------------------------------------
-- $env

data Entry = Entry
  { _time  :: SystemTime
  , _eHook :: Hook IO
  }
makeLenses ''Entry

instance HasHook Entry IO where hook = eHook

type Store = M.HashMap Unique Entry

-- | The 'Hooks' environment that is required for keeping track of all the
-- different targets and callbacks.
data Hooks = Hooks
  { eventSrc    :: EventSrc IO   -- ^ Where we get our events from
  , _injectTmr  :: TMVar Unique  -- ^ Used to signal timeouts
  , _hooks      :: TVar Store    -- ^ Store of hooks
  }
makeLenses ''Hooks

-- | Create a new 'Hooks' environment which reads events from the provided action
mkHooks' :: MonadUnliftIO m => EventSrc m -> m Hooks
mkHooks' s = withRunInIO $ \u -> do
  itr <- newEmptyTMVarIO
  hks <- newTVarIO M.empty
  pure $ Hooks (unliftESrc u s) itr hks

-- | Create a new 'Hooks' environment, but as a 'ContT' monad to avoid nesting
mkHooks :: MonadUnliftIO m => EventSrc m -> ContT r m Hooks
mkHooks = lift . mkHooks'

-- | Convert a hook in some UnliftIO monad into an IO version, to store it in Hooks
ioHook :: MonadUnliftIO m => Hook m -> m (Hook IO)
ioHook h = withRunInIO $ \u -> do

  t <- case _hTimeout h of
    Nothing -> pure Nothing
    Just t' -> pure . Just $ Timeout (t'^.delay) (u (_action t'))
  let f e = u $ _keyH h e
  pure $ Hook t f


--------------------------------------------------------------------------------
-- $op
--
-- The following code deals with simple operations on the environment, like
-- inserting and removing hooks.

-- | Insert a hook, along with the current time, into the store
register :: (HasLogFunc e)
  => Hooks
  -> Hook (RIO e)
  -> RIO e ()
register hs h = do
  -- Insert an entry into the store
  tag <- liftIO newUnique
  e   <- Entry <$> liftIO getSystemTime <*> ioHook h
  atomically $ modifyTVar (hs^.hooks) (M.insert tag e)
  -- If the hook has a timeout, start a thread that will signal timeout
  case h^.hTimeout of
    Nothing -> logDebug $ "Registering untimed hook: " <> display (hashUnique tag)
    Just t' -> do
      logDebug $ "Registering " <> display (t'^.delay)
              <> "ms hook: " <> display (hashUnique tag)
      void . async $ do
        threadDelay $ 1000 * fromIntegral (t'^.delay)
        atomically $ putTMVar (hs^.injectTmr) tag

-- | Run timeout action of hook and remove it from the store
runTimeout :: (HasLogFunc e)
  => Hooks
  -> Unique
  -> RIO e ()
runTimeout hs tag = do
  e <- atomically $ do
    m <- readTVar $ hs^.hooks
    let v = M.lookup tag m
    when (isJust v) $ modifyTVar (hs^.hooks) (M.delete tag)
    pure v
  case e of
    Nothing ->
      logDebug $ "Hook timeout ignored (already triggered): " <> display (hashUnique tag)
    Just e' -> do
      logDebug $ "Handeling timeout of hook: " <> display (hashUnique tag)
      liftIO $ e' ^. hTimeout . to fromJust . action


--------------------------------------------------------------------------------
-- $run
--
-- The following code deals with how we check hooks against incoming events, and
-- how this updates the 'Hooks' environment.

-- | Run the function stored in a Hook on the event and the elapsed time
runEntry :: HasLogFunc e => SystemTime -> KeyEvent -> (Unique, Entry) -> RIO e Catch
runEntry t e (k, v) = do
  logDebug $ "Hook " <> display (hashUnique k) <> ":"
  liftIO $ (v^.keyH) $ Trigger ((v^.time) `tDiff` t) e

-- | Run all hooks on the current event and reset the store
runHooks :: (HasLogFunc e)
  => Hooks
  -> KeyEvent
  -> RIO e (Maybe KeyEvent)
runHooks hs e = do
  logDebug $ "Running hooks for event: " <> display e
  m   <- atomically $ swapTVar (hs^.hooks) M.empty
  now <- liftIO getSystemTime
  foldMapM (runEntry now e) (M.toList m) >>= \case
    Catch   -> pure Nothing
    NoCatch -> pure $ Just e
  <* logDebug "Done running Hooks"


--------------------------------------------------------------------------------
-- $loop
--
-- The following code deals with how to use the 'Hooks' component as part of a
-- pull-chain. It contains logic for how to try to pull events from upstream and
-- check them against the hooks, and for how to keep stepping until an unhandled
-- event comes through.

-- | Pull 1 event from the '_eventSrc'. If that action is not caught by any
-- callback, then return it (otherwise return Nothing). At the same time, keep
-- reading the timer-cancellation inject point and handle any cancellation as it
-- comes up.
pull :: (HasLogFunc e)
  => Hooks                  -- ^ The 'Hooks' environment
  -> EventSrc (RIO e)       -- ^ An action that returns perhaps the next event
pull h@Hooks{eventSrc = EventSrc{tryESrc, postESrc}} = EventSrc
  { -- Handle any timer event first, and then try to read from the source
    tryESrc = (Left <$> takeTMVar (h^.injectTmr)) `orElse` (Right <$> tryESrc)

  -- Keep taking and cancelling timers until we encounter a key event, then run
  -- the hooks on that event.
  , postESrc = \case
    Left  t -> runTimeout h t $> Nothing -- We caught a hook timeout
    Right e -> liftIO (postESrc e) >>= maybe (pure Nothing) (runHooks h) -- We caught a real event
  }
