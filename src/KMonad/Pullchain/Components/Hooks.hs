{-|
Module      : KMonad.Pullchain.Components.Hooks
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
module KMonad.Pullchain.Components.Hooks
  ( Hooks
  , mkHooks
  , pull
  , register
  )
where

import KMonad.Prelude
import KMonad.Util.Logging

import Data.Time.Clock.System
import Data.Unique

import KMonad.Pullchain.Action
import KMonad.Pullchain.Types hiding (register)
import KMonad.Util hiding (time)

import RIO.Partial (fromJust)

import qualified RIO.HashMap as M
import qualified RIO         as R

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
  , _eHook :: Hook R.IO
  }
makeLenses ''Entry

instance HasHook Entry R.IO where hook = eHook

type Store = M.HashMap Unique Entry

-- | The 'Hooks' environment that is required for keeping track of all the
-- different targets and callbacks.
data Hooks = Hooks
  { _eventSrc   :: OnlyIO KeySwitch   -- ^ Where we get our events from
  , _injectTmr  :: TMVar Unique  -- ^ Used to signal timeouts
  , _hooks      :: TVar Store    -- ^ Store of hooks
  }
makeLenses ''Hooks

-- | Create a new 'Hooks' environment which reads events from the provided action
mkHooks' :: MonadUnliftIO m => m KeySwitch -> m Hooks
mkHooks' s = withRunInIO $ \u -> do
  itr <- atomically $ newEmptyTMVar
  hks <- atomically $ newTVar M.empty
  pure $ Hooks (u s) itr hks

-- | Create a new 'Hooks' environment, but as a 'ContT' monad to avoid nesting
mkHooks :: MonadUnliftIO m => m KeySwitch -> Ctx r m Hooks
mkHooks = lift . mkHooks'

-- | Convert a hook in some UnliftIO monad into an IO version, to store it in Hooks
ioHook :: MonadUnliftIO m => Hook m -> m (Hook R.IO)
ioHook h = withRunInIO $ \u -> do

  t <- case _hTimeout h of
    Nothing -> pure Nothing
    Just t' -> pure . Just $ Timeout (t'^.delay) (u (_action t'))
  let f = \e -> u $ (_keyH h) e
  pure $ Hook t f


--------------------------------------------------------------------------------
-- $op
--
-- The following code deals with simple operations on the environment, like
-- inserting and removing hooks.

-- | Insert a hook, along with the current time, into the store
register :: LUIO m e
  => Hooks
  -> Hook m
  -> m ()
register hs h = do
  -- Insert an entry into the store
  tag <- liftIO newUnique
  e   <- Entry <$> liftIO getSystemTime <*> ioHook h
  atomically $ modifyTVar (hs^.hooks) (M.insert tag e)
  -- If the hook has a timeout, start a thread that will signal timeout
  case h^.hTimeout of
    Nothing -> logDebug $ "Registering untimed hook: " <> tshow (hashUnique tag)
    Just t' -> void . async $ do
      logDebug $ "Registering " <> tshow (t'^.delay)
              <> "ms hook: " <> tshow (hashUnique tag)
      threadDelay $ 1000 * (fromIntegral $ t'^.delay)
      atomically $ putTMVar (hs^.injectTmr) tag

-- | Cancel a hook by removing it from the store
cancelHook :: LIO m e
  => Hooks
  -> Unique
  -> m ()
cancelHook hs tag = do
  e <- atomically $ do
    m <- readTVar $ hs^.hooks
    let v = M.lookup tag m
    when (isJust v) $ modifyTVar (hs^.hooks) (M.delete tag)
    pure v
  case e of
    Nothing ->
      logDebug $ "Tried cancelling expired hook: " <> tshow (hashUnique tag)
    Just e' -> do
      logDebug $ "Cancelling hook: " <> tshow (hashUnique tag)
      liftIO $ e' ^. hTimeout . to fromJust . action


--------------------------------------------------------------------------------
-- $run
--
-- The following code deals with how we check hooks against incoming events, and
-- how this updates the 'Hooks' environment.

-- | Run the function stored in a Hook on the event and the elapsed time
runEntry :: MonadIO m => SystemTime -> KeySwitch -> Entry -> m Catch
runEntry t e v = liftIO $ do
  (v^.keyH) $ Trigger ((v^.time) `tDiff` t) e

-- | Run all hooks on the current event and reset the store
runHooks :: LIO m e
  => Hooks
  -> KeySwitch
  -> m (Maybe KeySwitch)
runHooks hs e = do
  logDebug "Running hooks"
  m   <- atomically $ swapTVar (hs^.hooks) M.empty
  now <- liftIO getSystemTime
  foldMapM (runEntry now e) (M.elems m) >>= \case
    Catch   -> pure $ Nothing
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
step :: LUIO m e
  => Hooks               -- ^ The 'Hooks' environment
  -> m (Maybe KeySwitch) -- ^ An action that returns perhaps the next event
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

-- | Keep stepping until we succesfully get an unhandled 'KeySwitch'
pull' :: LUIO m e
  => Hooks
  -> m KeySwitch
pull' h = step h >>= maybe (pull' h) pure

-- | Keep stepping until we succesfully get an unhandled 'KeySwitch'
--
-- NOTE: This is just a temp fix to get things to compile while we're
-- reorganizing the code. This should all be replaced by a new model at some
-- point.
pull :: LIO m e
  => Hooks
  -> m KeySwitch
pull h = view logEnv >>= \env -> runRIO env (pull' h)
