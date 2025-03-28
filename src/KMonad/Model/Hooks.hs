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

import Data.Time.Clock.System
import Data.Unique

import KMonad.Model.Action hiding (register)
import KMonad.Keyboard

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
  { _eventSrc   :: IO KeyEvent   -- ^ Where we get our events from
  , _injectTmr  :: TMVar Unique  -- ^ Used to signal timeouts
  , _hooks      :: TVar Store    -- ^ Store of hooks
  }
makeLenses ''Hooks

-- | Create a new 'Hooks' environment which reads events from the provided action
mkHooks' :: MonadUnliftIO m => m KeyEvent -> m Hooks
mkHooks' s = withRunInIO $ \u ->
  Hooks (u s) <$> newEmptyTMVarIO <*> newTVarIO M.empty

-- | Create a new 'Hooks' environment, but as a 'ContT' monad to avoid nesting
mkHooks :: MonadUnliftIO m => m KeyEvent -> ContT r m Hooks
mkHooks = lift . mkHooks'

-- | Convert a hook in some UnliftIO monad into an IO version, to store it in Hooks
ioHook :: MonadUnliftIO m => Hook m -> m (Hook IO)
ioHook (Hook t f) = withRunInIO $ \u -> pure $ Hook (t & _Just.action %~ u) (u . f)


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
    Just t' -> void . async $ do
      logDebug $ "Registering " <> display (t'^.delay)
              <> "ms hook: " <> display (hashUnique tag)
      threadDelay $ 1000 * fromIntegral (t'^.delay)
      atomically $ putTMVar (hs^.injectTmr) tag

-- | Cancel a hook by removing it from the store
cancelHook :: (HasLogFunc e)
  => Hooks
  -> Unique
  -> RIO e ()
cancelHook hs tag = do
  e <- atomically $ stateTVar (hs^.hooks) $ at tag <<.~ Nothing
  case e of
    Nothing ->
      logDebug $ "Tried cancelling expired hook: " <> display (hashUnique tag)
    Just e' -> do
      logDebug $ "Cancelling hook: " <> display (hashUnique tag)
      liftIO $ e' ^. hTimeout . _Just . action


--------------------------------------------------------------------------------
-- $run
--
-- The following code deals with how we check hooks against incoming events, and
-- how this updates the 'Hooks' environment.

-- | Run the function stored in a Hook on the event and the elapsed time
runEntry :: MonadIO m => SystemTime -> KeyEvent -> Entry -> m Catch
runEntry t e v = liftIO $ do
  (v^.keyH) $ Trigger ((v^.time) `tDiff` t) e

-- | Run all hooks on the current event and reset the store
runHooks :: (HasLogFunc e)
  => Hooks
  -> KeyEvent
  -> RIO e (Maybe KeyEvent)
runHooks hs e = do
  logDebug "Running hooks"
  m   <- atomically $ swapTVar (hs^.hooks) M.empty
  now <- liftIO getSystemTime
  foldMapM (runEntry now e) (M.elems m) >>= \case
    Catch   -> pure Nothing
    NoCatch -> pure $ Just e


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
step :: (HasLogFunc e)
  => Hooks                  -- ^ The 'Hooks' environment
  -> RIO e (Maybe KeyEvent) -- ^ An action that returns perhaps the next event
step h = do

  -- Asynchronously start reading the next event
  a <- async . liftIO $ h^.eventSrc

  -- Handle any timer event first, and then try to read from the source
  let next = (Left <$> takeTMVar (h^.injectTmr)) `orElse` (Right <$> waitSTM a)

  -- Keep taking and cancelling timers until we encounter a key event, then run
  -- the hooks on that event.
  let read = atomically next >>= \case
        Left  t -> cancelHook h t >> read -- We caught a cancellation
        Right e -> runHooks h e           -- We caught a real event
  read

-- | Keep stepping until we succesfully get an unhandled 'KeyEvent'
pull :: HasLogFunc e
  => Hooks
  -> RIO e KeyEvent
pull h = step h >>= maybe (pull h) pure
