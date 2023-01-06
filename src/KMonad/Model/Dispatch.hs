{-
Module      : KMonad.Model.Dispatch
Description : Component for async reading.
Copyright   : (c) David Janssen, 2019
License     : MIT
Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : portable

The 'Dispatch' component of the app-loop solves the following problem: we might
at some point during execution be in the following situation:
- We have set our processing to held
- There is a timer running that might unhold at any point
- We are awaiting a key from the OS

This means we need to be able to:
1. Await events from some kind of rerun buffer
2. Await events from the OS
3. Do both of these things without ever entering a race-condition where we lose
   an event because both 1. and 2. happen at exactly the same time.

The Dispatch component provides the ability to read events from some IO action
while at the same time providing a method to write events into the Dispatch,
sending them to the head of the read-queue, while guaranteeing that no events
ever get lost.

In the sequencing of components, the 'Dispatch' occurs first, which means that
it reads directly from the KeySource. Any component after the 'Dispatch' need
not worry about wether an event is being rerun or not, it simply treats all
events as equal.

-}
module KMonad.Model.Dispatch
  ( -- $env
    Dispatch
  , mkDispatch

    -- $op
  , pull
  , rerun
  )
where

import KMonad.Prelude
import KMonad.Keyboard
import KMonad.Model.Action (WrappedEvent(..), Catch(..))

import Data.Unique
import RIO.Seq (Seq(..), (><))
import qualified RIO.Seq  as Seq
import qualified RIO.Text as T

--------------------------------------------------------------------------------
-- $env
--
-- The 'Dispatch' environment, describing what values are required to perform
-- the Dispatch operations, and constructors for creating such an environment.

-- | The 'Dispatch' environment
data Dispatch = Dispatch
  { _eventSrc :: IO KeyEvent            -- ^ How to read 1 event
  , _readProc :: TMVar (Async KeyEvent) -- ^ Store for reading process
  , _rerunBuf :: TVar (Seq WrappedEvent)    -- ^ Buffer for rerunning events
  , _injectTmr :: TMVar Unique -- ^ Used to signal timeouts
  }
makeLenses ''Dispatch

-- | Create a new 'Dispatch' environment
mkDispatch' :: MonadUnliftIO m => m KeyEvent -> TMVar Unique -> m Dispatch
mkDispatch' s itr = withRunInIO $ \u -> do
  rpc <- atomically $ newEmptyTMVar
  rrb <- atomically $ newTVar Seq.empty
  pure $ Dispatch (u s) rpc rrb itr

-- | Create a new 'Dispatch' environment in a 'ContT' environment
mkDispatch :: MonadUnliftIO m => m KeyEvent -> TMVar Unique -> ContT r m Dispatch
mkDispatch s itr = lift (mkDispatch' s itr)

--------------------------------------------------------------------------------
-- $op
--
-- The supported 'Dispatch' operations.

-- | Return the next event, this will return either (in order of precedence):
-- 1. The next item to be rerun
-- 2. A new item read from the OS
-- 3. Pausing until either 1. or 2. triggers
pull :: (HasLogFunc e) => Dispatch -> RIO e WrappedEvent
pull d = do
  -- Check for an unfinished read attempt started previously. If it exists,
  -- fetch it, otherwise, start a new read attempt.
  a <- atomically (tryTakeTMVar $ d^.readProc) >>= \case
    Nothing -> async . liftIO $ d^.eventSrc
    Just a' -> pure a'

  -- First try reading from the rerunBuf, or failing that, from the
  -- read-process. If both fail we enter an STM race.
  atomically (
    (Left <$> takeTMVar (d^.injectTmr))
    `orElse`(Right . Left <$> popRerun)
    `orElse` (Right . Right <$> waitSTM a)
    ) >>= \case
      -- If there's a timer, pass it through
      Left t -> do
        atomically $ putTMVar (d^.readProc) a
        pure (WrappedTag t)
      -- If we take from the rerunBuf, put the running read-process back in place
      Right (Left e') -> do
        logDebug $ "\n" <> display (T.replicate 80 "-")
                <> "\nRerunning event: " <> display e'
        atomically $ putTMVar (d^.readProc) a
        pure e'
      Right (Right e') -> pure (WrappedKeyEvent NoCatch e')

  where
    -- Pop the head off the rerun-buffer (or 'retrySTM' if empty)
    popRerun = readTVar (d^.rerunBuf) >>= \case
      Seq.Empty -> retrySTM
      (e :<| b) -> do
        writeTVar (d^.rerunBuf) b
        pure e

-- | Add a list of elements to be rerun.
rerun :: (HasLogFunc e) => Dispatch -> [WrappedEvent] -> RIO e ()
rerun d es = atomically $ modifyTVar (d^.rerunBuf) (>< Seq.fromList es)
