module KMonad.Daemon.Dispatch

where

import Prelude

import Control.Monad.Except
import Data.Semigroup (Any(..))
import Data.Unique
import GHC.Conc (orElse)

import KMonad.Event
import KMonad.Keyboard
import KMonad.Util

import qualified KMonad.Button.Action as Ac

-- TODO: potential improvement:
--   - replace rerunBuf with a TVar of Sequence instead of a TChan.
--   - replace blockBuf with a check on 'blocked'



--------------------------------------------------------------------------------

data Dispatch = Dispatch
  { _keysIn       :: TChan KeyEvent
    -- ^ Where raw KeyIO gets written to
  , _rerunBuf     :: TChan KeyEvent
    -- ^ Used to rerun events after a block is released
  , _capturePoint :: TMVar (KeyEvent -> IO ())
    -- ^ Used to capture the event stream for external processes
  , _callbacks    :: TVar [Callback]
    -- ^ Used to provide 'catchNext' and 'catchWithin' behavior
  , _blocked      :: TVar Bool
    -- ^ Used to block and unblock processing
  , _blockBuf     :: TVar [KeyEvent]
    -- ^ Used to temporarily store events while blocked
  , _injectV      :: TMVar Event
    -- ^ Used to inject events into the eventloop
  }
makeClassy ''Dispatch

runCallbacks = undefined


-- | getKey is an STM transaction across the state stored in the Daemon. It
-- attempts to read in a new KeyEvent, an attempt that might fail if:
--
-- 1. The input is currently captured by a process
-- 2. The input is captured by some callback
-- 3. We are in a 'blocked' state
--
-- This call will block until an input event is available (either immediately
-- from the rerunBuf, or until the OS registers a keyboard event). It will
-- subsequently return a Maybe KeyEvent (Nothing means that it was captured
-- somewhere in the above-mentioned steps). Additionally, it returns an IO
-- action that is to be executed upon returning. It contains all IO operations
-- requested by the callbacks.
getKey :: Dispatch -> STM (Maybe KeyEvent, IO ())
getKey d = do
  -- Read from the rerunBuf first, failing that, wait for new input.
  e <- readTChan (d^.rerunBuf) `orElse` readTChan (d^.keysIn)

  -- If any 'event-capture' function is registered, use that to dispatch the
  -- event and don't return it.
  let checkCapture = ExceptT $ do
        tryTakeTMVar (d^.capturePoint) >>= \case
          Just f  -> pure . Left  $ f e
          Nothing -> pure . Right $ (pure () :: IO ())

  -- Try all callbacks, updating the store and collecting all actions. If any
  -- callback captures, don't return the event.
  let checkCallbacks = ExceptT $ do
        (isCap, cs, io) <- runCallbacks e <$> (readTVar $ d^.callbacks)
        writeTVar (d^.callbacks) cs
        if getAny isCap
          then pure . Left  $ io
          else pure . Right $ io

  -- Check to see if we are blocked. If so, then add event to blockBuf.
  -- Otherwise return the event. Either way, pass on the action from
  -- checkCallbacks.
  let checkBlocked io = ExceptT $ do
        readTVar (d^.blocked) >>= \case
          True -> do modifyTVar (d^.blockBuf) (e:)
                     pure . Left $ io
          False -> pure . Right $ io

  -- Chain all the actions and dispatch on the results
  runExceptT (checkCapture >> checkCallbacks >>= checkBlocked) >>= \case
    Left  io -> pure (Nothing, io)
    Right io -> pure (Just e,  io)


--------------------------------------------------------------------------------
-- $ops
--

-- | Pause or unpause the stream
--
-- If we pause the stream, we simply put the 'Sluice' into blocked mode, any new
-- events will be stored in the sluice until we unblock it.
--
-- If we unpause the stream, in the same atomic action we unblock the sluice and
-- prepend any stored events to rerunBuffer. The next sluice-read event will
-- then pull from the head of the rerunBuffer again.
hold :: HasDispatch e => Bool -> RIO e ()
hold True  = view blocked >>= \b -> atomically $ writeTVar b True
hold False = view dispatch >>= \d -> atomically $ do
  writeTVar (d^.blocked) True
  es <- swapTVar (d^.blockBuf) []
  traverse_ (unGetTChan $ d^.rerunBuf) es

catchNext :: HasDispatch e => Milliseconds -> Ac.MatchFun m -> Ac.OnMatch m -> RIO e ()
catchNext ms p f = do
  undefined



-- -- | Inject an event into the Dispatch object
-- inject :: HasDispatch e => Event -> RIO e ()
-- inject e = view injectP >>= flip Ip.inject e

-- -- | Register a 'KeyEvent' intercept
-- intercept :: HasDispatch e
--   => B.Callback -> RIO e ()
-- intercept c = view cReg >>= flip modifyMVar_ (pure . (c:))

-- -- | Wait for the next Event
-- awaitEvent :: HasDispatch e => RIO e Event
-- awaitEvent = view injectP >>= Ip.read
