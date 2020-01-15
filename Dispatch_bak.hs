module KMonad.Components.Dispatch
  -- (
  --   -- * The Dispatch component
  --   -- $disp
  --   Dispatch
  -- , HasDispatch
  -- , withDispatch
  -- , dispatch

  --   -- * Working with input
  -- , StreamLoc(..)
  -- , injectEvent
  -- , awaitEvent
  -- , pauseStream
  -- , intercept
  -- , copyStream
  -- , captureStream
  -- )
where

import Prelude

import Control.Concurrent.Chan.Unagi

import KMonad.Event
import KMonad.Keyboard
import KMonad.Keyboard.IO
import KMonad.Util
import KMonad.Runner


import qualified Data.CapturePoint as C
import qualified Data.Sluice as L


--------------------------------------------------------------------------------
-- $intercept
--
-- Intercepts are functions on 'KeyEvent's that contain a callback. They
-- function to short-circuit the normal event-processing of KMonad by sending
-- input directly from Key IO to the callback, bypassing the blocking
-- functionality of KMonad, and also some of the input capture mechanisms. See
-- the documentation for more information on the exact architecture.
--
-- All registered intercepts are matched against input, any intercept that
-- matches has its callback performed. All intercepts that match are
-- automatically unregistered. If any intercept matches, the key event is no
-- longer propagated through KMonad.

-- | A 'Callback' is some effectful function on a KeyEvent that returns the
-- KeyEvent when finished.


-- | I wonder if this can be done without the MVar...
data Intercept = Intercept
  { _predicate :: KeyEvent -> Bool
  , _comms     :: MVar KeyEvent
  }
makeLenses ''Intercept

mkIntercept :: MonadUnliftIO m => (KeyEvent -> Bool) -> m Intercept
mkIntercept p = Intercept p <$> newEmptyMVar


-- | Check all the Intercept predicates against a KeyEvent. Separating those
-- that match it from those that don't.
runIntercepts :: KeyEvent -> [Intercept] -> ([MVar KeyEvent], [Intercept])
runIntercepts e = partitionEithers . map f
  where f i = if (i^.predicate $ e) then Left $ i^.comms else Right i


--------------------------------------------------------------------------------
-- $disp
--

-- | The Dispatch type that contains all the information needed to keep track of
-- the various event blocking, filtering, and capturing that is provided by
-- KMonad.
--

data Dispatch = Dispatch
  { _input       :: C.CapturePoint KeyEvent
  , _intercepts  :: !(MVar [Intercept])
  , _sluice      :: !(L.Sluice KeyEvent)
  , _injectPoint :: !(MVar Event)
  }
makeClassy ''Dispatch


withDispatch :: (HasRunEnv e)
  => KeySource
  -> ContT r (RIO e) Dispatch
withDispatch src = do
  cpp <- lift $ C.mkCapturePoint
  ics <- lift $ newMVar []
  ijp <- lift $ newEmptyMVar

  slc <- L.mkSluice

  -- Launch a thread that copies key IO into the CapturePoint immediately
  launch_ "dispatch:keyio-thread" $
    awaitKeyWith src >>= C.write cpp

  -- Return the dispatch
  pure $ Dispatch cpp ics slc ijp




--     -- let (matches, st') = runIntercepts e st
--     -- if null matches
--     --   then pure (st', Just e)
--     --   else traverse_ (liftIO . ($ e)) callbacks >> pure (st', Nothing)


-- --------------------------------------------------------------------------------
-- -- $ops
-- --
-- -- Operations inside an environment that has a 'Dispatch' in its envirmonment

-- -- | Inject an event into KMonad's app-loop. Injected events completely bypass
-- -- all blocking or capturing mechanisms and are only visible from within KMonad.
-- injectEvent :: HasDispatch e => Event -> RIO e ()
-- injectEvent e = view evInChan >>= liftIO . flip writeChan e

-- -- | Wait for an event to occur and then return it. This will return any
-- -- injected event, or any Key IO event that has not been blocked or intercepted.
-- awaitEvent :: HasDispatch e => RIO e Event
-- awaitEvent = view evOutChan >>= liftIO . readChan

-- -- | Pause or unpause event processing (the pause occurs after callbacks, so any
-- -- callback will still register immediately.)
-- pauseStream :: HasDispatch e => Bool -> RIO e ()
-- pauseStream b = view sluice >>= \s -> if b then L.block s else L.unblock s

-- -- | The 'StreamLoc' datatype distinguishes between 2 locations where a stream
-- -- of events can be either copied or captured. The 'Early' location occurs
-- -- immediately after a KeyEvent is written to KMonad by the OS. This completely
-- -- bypasses all callback handling and blocking behavior. The 'Late' location
-- -- occurs after both the callbacks and the blocking behavior has been performed,
-- -- so that stream will be filtered of callbacks and will be paused and unpaused
-- -- when KMonad pauses and unpauses.
-- data StreamLoc = Early | Late deriving (Eq, Show)

-- -- | Intercept a 'KeyEvent' matching a predicate just after the 'Early' capture
-- -- node. This will block until the predicate is matched.
-- intercept :: HasDispatch e
--   => (KeyEvent -> Bool)  -- ^ The predicate to match against 'KeyEvent'
--   -> RIO e KeyEvent      -- ^ Return the waiting action.
-- intercept p = do
--   v <- view interceptStore
--   i <- mkIntercept p
--   modifyMVar_ v (\is -> pure (i:is))
--   takeMVar (i^.comms)


-- -- | Capture either the `early` or `late` stream.
-- --
-- -- All 'KeyEvent's from the chosen location onwards will be redirected to the
-- -- 'Captured' until 'release' is called on it.
-- captureStream :: HasDispatch e
--   => StreamLoc                   -- ^ Whether to capture the early or late stream
--   -> RIO e (C.Captured KeyEvent) -- ^ An action that returns the requested stream
-- captureStream loc = C.captureStream =<< case loc of
--     Early -> view rawChan
--     Late  -> view blockChan

-- -- | Get a copy of either the `early` or `late` stream (see 'StreamLoc' for more
-- -- explanation on the difference). Copies of input streams do not interfere with
-- -- the other streams at all. When the stream copied by 'copyStream' is later
-- -- captured in some other location by 'captureStream' the copy will also block
-- -- until the capture is released.
-- copyStream :: HasDispatch e
--   => StreamLoc
--   -> RIO e (RIO e KeyEvent)
-- copyStream loc = C.copyStream =<< case loc of
--   Early -> view rawChan
--   Late  -> view blockChan
