{-|
Module      : KMonad.App.Hooks
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
module KMonad.App.Hooks
  ( Hooks
  , mkHooks
  , pull
  , hookNext
  , hookWithin
  )
where

import KMonad.Prelude

import Data.Semigroup (Any(..))
import Data.Time.Clock.System
import Data.Unique

import KMonad.Action hiding (hookNext, hookWithin)
import KMonad.Keyboard
import KMonad.Util

import qualified RIO.HashMap as M
import qualified RIO.Text    as T

--------------------------------------------------------------------------------
-- $hooks

-- | A 'Hook' contains the 'KeyPred' and 'Callback'
newtype Hook = Hook (KeyPred, Callback IO)
makeWrapped ''Hook

-- | Create a new 'Hook' value
mkHook :: MonadUnliftIO m => KeyPred -> Callback m -> m Hook
mkHook p c = withRunInIO $ \u -> pure $ Hook (p, (u . c))


--------------------------------------------------------------------------------
-- $env

-- | The 'Hooks' environment that is required for keeping track of all the
-- different targets and callbacks.
data Hooks = Hooks
  { _eventSrc   :: IO KeyEvent                  -- ^ Where we get our events from
  , _injectTmr  :: TMVar Unique                 -- ^ Used to signal timeouts
  , _nextHooks  :: TVar [Hook]                  -- ^ Hooks to run on next event
  , _timerHooks :: TVar (M.HashMap Unique Hook) -- ^ Storage for timer hooks
  }
makeLenses ''Hooks

-- | Create a new 'Hooks' environment which reads events from the provided action
mkHooks' :: MonadUnliftIO m => m KeyEvent -> m Hooks
mkHooks' s = withRunInIO $ \u -> do
  itr <- atomically $ newEmptyTMVar
  nhs <- atomically $ newTVar []
  ths <- atomically $ newTVar M.empty
  pure $ Hooks (u s) itr nhs ths

-- | Create a new 'Hooks' environment, but as a 'ContT' monad to avoid nesting
mkHooks :: MonadUnliftIO m => m KeyEvent -> ContT r m Hooks
mkHooks = lift . mkHooks'


--------------------------------------------------------------------------------
-- $op
--
-- The following code deals with simple operations on the environment, like
-- inserting and removing hooks.

-- | Implementation of 'hookNext' from "KMonad.Action".
--
-- Add a hook to be called on the next event.
hookNext :: (HasLogFunc e)
  => Hooks            -- ^ The 'Hooks' environment
  -> KeyPred          -- ^ Predicate to match with
  -> Callback (RIO e) -- ^ The callback to run against the match
  -> RIO e ()         -- ^ The registering action
hookNext h p a = do
  logDebug "Registering <NEXT> hook"
  nh <- mkHook p a
  atomically $ modifyTVar (h^.nextHooks) (nh:)

-- | Implementation of 'hookWithin' from "KMonad.Action".
--
-- Add a timer-hook with a unique identifier to the 'Hooks' environment, and
-- starts a process that will trigger a timeout.
hookWithin :: (HasLogFunc e)
  => Hooks
  -> Milliseconds
  -> KeyPred
  -> Callback (RIO e)
  -> RIO e ()
hookWithin h ms p a = do
  logDebug $ "Registering <TIMER> hook with: " <> display ms <> "ms"
  kh <- mkHook p a
  tg <- liftIO newUnique
  atomically $ modifyTVar (h^.timerHooks) (M.insert tg kh)
  void . async $ do
    threadDelay $ 1000 * (fromIntegral ms)
    atomically $ putTMVar (h^.injectTmr) tg

-- | Try to cancel a hook stored in 'timerHooks'.
--
-- If it doesn't exist (due to a race), do nothing. If it does, remove it from
-- the map and perform its callback on a NoMatch.
cancelTimer :: (HasLogFunc e) => Hooks -> Unique -> RIO e ()
cancelTimer h t = withRunInIO $ \u -> do
  join . atomically $ do
    m <- readTVar $ h^.timerHooks
    let cncl (Hook (_, go)) = do
          modifyTVar (h^.timerHooks) (M.delete t)
          pure . liftIO $ do
            u . logDebug $ display (T.replicate 80 "-")
                        <> "\nCancelling <TIMER> hook"
            void $ go NoMatch
    maybe (pure $ pure ()) cncl $ M.lookup t m

--------------------------------------------------------------------------------
-- $run
--
-- The following code deals with how we check hooks against incoming events, and
-- how this updates the 'Hooks' environment.

-- | Print out information about hooks and matches
debugReport :: Hooks -> KeyEvent -> RIO e Text
debugReport h _ = do

  -- Generate the report for the 'NEXT' hooks
  nhs <- atomically . readTVar $ h^.nextHooks
  let ntxt = "Running " <> textDisplay (length nhs) <> " <NEXT> hooks\n"

  -- Generate the report for the 'TIMER' hooks
  ths <- M.elems <$> (atomically . readTVar $ h^.timerHooks)
  let ttxt = "Running " <> textDisplay (length ths) <> " <TIMER> hooks"

  pure $ ntxt <> ttxt

-- | Run a 'Hook' from 'hookNext'
runNextHook :: ()
  => KeyEvent -- ^ The 'KeyEvent' to compare against
  -> Hook     -- ^ The 'Hook' to use
  -> IO Catch -- ^ Monoidal result
runNextHook e (Hook (p, f)) = f . p $ e

-- | Run timer 'Hook' on a 'KeyEvent'
--
-- This takes a 'KeyEvent' and a 'Hook' and returns a result tuple consisting
-- of:
-- 1. Either an empty (matched) or singleton (no match) hashmap with the id and timer
-- 2. Either the IO action to run on the Event or else an empty IO action
runTimerHook :: ()
  => KeyEvent                          -- ^ The 'KeyEvent to compare against'
  -> (Unique, Hook)                    -- ^ The timer 'Hook' and its 'Unique' identifier
  -> (M.HashMap Unique Hook, IO Catch) -- ^ Monoidal result
runTimerHook e (u, it@(Hook (p, f))) = case m of
  Match _ -> (M.empty, f m)
  NoMatch -> (M.singleton u it, pure NoCatch)
  where m = p e

-- | Run all hooks on the provided keyevent.
--
-- NOTE: The hooks are run very differently:
--
-- - A /next/ 'Hook' is run only once, if it matches, its 'Match' action is run,
--   if it doesn't match, its 'NoMatch' action is run. Either way, the hook is
--   removed from the 'Hooks' action is run.
--
-- - A /timer/ 'Hook' is checked against any Event as long as it remains in the
--   'Hooks'. The moment a timer hook matches an input, its 'Match' action is
--   run, and it is removed. However, when a timer doesn't match on a Event, no
--   action is performed, but the hook remains in the store. The way a /timer/
--   hook registers a 'NoMatch' is when its timer-event occurs when it is still
--   in the 'Hooks'.
runHooks :: (HasLogFunc e) => Hooks -> KeyEvent -> RIO e (Maybe KeyEvent)
runHooks h e = do
  logDebug . display =<< debugReport h e

  -- NOTE: this happens atomically in STM, so we will never be interrupted by a
  -- timer terminating in the middle of this operation.
  c <- liftIO . join . atomically $ do
    -- These foldMap calls use the monoidal nature of the 'Catch' results
    nio       <- foldMap (runNextHook e) <$> swapTVar (h^.nextHooks) []
    (ts, tio) <- foldMap (runTimerHook e) . M.toList <$> readTVar (h^.timerHooks)
    writeTVar (h^.timerHooks) ts
    pure $ nio <> tio

  pure $ if c == Catch then Nothing else Just e


--------------------------------------------------------------------------------
-- $loop
--
-- The following code deals with how to use the 'Hooks' component as part of a
-- pull-chain. It contains logic for how to try to pull events from upstream and
-- check them against the hooks, and for how to keep stepping until an unhandled
-- event comes through.

-- | Pull 1 event from the 'eventSrc'. If that action is not caught by any
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
        Left  t -> cancelTimer h t >> read -- We caught a cancellation
        Right e -> runHooks h e            -- We caught a real event
  read

-- | Keep stepping until we succesfully get an unhandled 'KeyEvent'
pull :: HasLogFunc e
  => Hooks
  -> RIO e KeyEvent
pull h = step h >>= maybe (pull h) pure
