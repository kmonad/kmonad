module KMonad.Components.Dispatch
  -- ( -- * The Dispatch component
  --   -- $disp
  --   Dispatch
  -- , HasDispatch
  -- , mkDispatch
  -- , inputDispatch

  --   -- * Working with input
  -- , injectEvent
  -- , awaitEvent
  -- , copyStream
  -- , captureStream
  -- )
where

import Prelude

import Control.Concurrent.Chan.Unagi

import KMonad.Button
import KMonad.Event
import KMonad.Keyboard
import KMonad.Keyboard.IO
import KMonad.Util
import KMonad.Runner

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

data Intercept = Intercept
  { _predicate :: KeyEvent -> Bool
  , _callback  :: KeyEvent -> Action
  }

--------------------------------------------------------------------------------
-- $chan
--
-- A 'CChan' is a 'Chan'-style data-type that provides a mechanism for someone
-- to either copy or capture the data-stream. A `capture` will return an action
-- that reads events from the channel, and an action to `uncapture` it again
--
--

data Capture ...
   { read
   , release
   }
etc

CONTINUE HERE

-- 'CChan' (capture-chan) is a simple wrapper over 'Chan' with support for
-- copying or capturing the output stream. A blocking read of this 'CChan' will

-- | The 'CChan' datatype
data CChan a = CChan
  { _cIn  :: !(InChan a)
  , _cOut :: !(MVar (OutChan a))
  }
makeLenses ''CChan

-- | Create a new 'CChan'
mkCChan :: MonadUnliftIO m => m (CChan a)
mkCChan = do
  (i, o) <- liftIO newChan
  CChan i <$> newMVar o

-- | Turn an OutChan into an action that reads from an OutChan, this way we can
-- hide the Chan implementation completely inside CChan
reading :: MonadUnliftIO m => OutChan a -> m a
reading = liftIO . readChan

-- | Inject a new thing into the front of the 'CChan'
writeCChan :: MonadUnliftIO m => a -> CChan a -> m ()
writeCChan a = liftIO . flip writeChan a . view cIn

-- | Blocking raid awaiting the arrival of a thing from the 'CChan'
readCChan :: MonadUnliftIO m => CChan a -> m a
readCChan = (flip withMVar $ liftIO . readChan) . view cOut

-- | Get a copy of the output stream
copyStream :: MonadUnliftIO m => CChan a -> m (m a)
copyStream c = do
  mo <- liftIO . dupChan $ c^.cIn
  pure $ reading mo

-- | Capture the stream, giving an OutChan to read from,
captureStream :: MonadUnliftIO m => CChan a -> m (m a, m ())
captureStream c = do
  o <- takeMVar $ c^.cOut
  pure (reading o, putMVar (c^.cOut) o)


--------------------------------------------------------------------------------
-- $disp
--
-- The 'Dispatch' component is responsible for managing the inputs coming
-- in from the OS'es keyboard. It provides numerous functionalities:
--   - injecting events into KMonad
--   - copying or capturing input streams
--   - blocking or unblocking processing
--   - handling input-short circuiting


-- | The environment for 'Dispatch' functionality.
--
-- The 'Dispatch' functions thusly:
-- 1. KeyEvents from the OS are written to 'keyIn'
-- 2. KeyEvents are read from keyOut. This channel can be copied or captured.
-- 3. KeyEvents are checked against intercepts, maybe capturing them.
-- 4. KeyEvents are written to the Sluice, which can be blocked or open
-- 5. Once the sluice is open, all events are written to eventIn
-- 6. Events can also be injected into eventIn
-- 7. Events are read from eventOut to the app-loop, this channel can be copied
--    or captured.
--   

data Dispatch = Dispatch
  { _keyChan        :: CChan KeyEvent
  , _helpChan       :: CChan KeyEvent
  , _eventChan      :: CChan Event
  , _interceptStore :: !(MVar [Intercept])
  , _sluice         :: !(MVar (L.Sluice KeyEvent))
  }
makeClassy ''Dispatch


-- | Create a new 'Dispatch' object
mkDispatch :: (HasRunEnv e)
  => KeySource      -- ^ The source from which we read OS 'KeyEvent's
  -> RIO e Dispatch
mkDispatch src = do
  -- Initialize all the components
  dsp <- Dispatch <$> mkCChan <*> mkCChan <*> mkCChan <*> newMVar [] <*> newMVar L.empty

  -- Launch the thread that writes Key IO to keyChan
  a <- async . forever $ do
    e <- awaitKeyWith src
    writeCChan e (dsp^.keyChan)
  link a      -- <-- If reading ever crashes, so do we

  -- Launch the thread that writes

  pure dsp

-- | Inject an event into KMonad's app-loop
injectEvent :: HasDispatch e => Event -> RIO e ()
injectEvent e = view eventChan >>= writeCChan e

-- | Wait for an event to occur and then return it
awaitEvent :: HasDispatch e => RIO e Event
awaitEvent = view eventChan >>= readCChan

data CopyType = Copy | Capture deriving (Eq, Show)
data

-- -- | Get a copy of the event-stream
-- copyStream :: HasDispatch e => RIO e (OutChan Event)
-- copyStream = view inChan >>= liftIO . dupChan

-- -- | Capture the event-stream. This will block any and all processing of events
-- -- in KMonad until the release action is performed. This returns the channel
-- -- from which to read events, and the action to release the channel again.
-- --
-- -- Note: this does not stop any copies of the main stream from functioning. That
-- -- functionality is not supported.
-- --
-- captureStream :: HasDispatch e => RIO e (OutChan Event, RIO e ())
-- captureStream = do
--   oV <- view outChan
--   o  <- takeMVar oV
--   pure $ (o, putMVar oV o)
