module KMonad.Components.InputDispatch
  ( -- * The InputDispatch component
    -- $disp
    InputDispatch
  , HasInputDispatch
  , mkInputDispatch
  , inputDispatch

    -- * Working with input
  , injectEvent
  , awaitEvent
  , copyStream
  , captureStream
  )
where

import KMonad.Prelude

import Control.Concurrent.Chan.Unagi

import KMonad.Event
import KMonad.Keyboard.IO


--------------------------------------------------------------------------------
-- $disp
--
-- The 'InputDispatch' component is responsible for managing the inputs coming
-- in from the OS'es keyboard. It provides functionality for injecting events
-- into KMonad, and for copying or capturing the input-stream for use in
-- external scripts.

-- | The environment for 'InputDispatch' functionality
data InputDispatch = InputDispatch
  { _inChan  :: InChan Event         -- ^ Where new events get written to
  , _outChan :: MVar (OutChan Event) -- ^ A channel that provides events
  }
makeClassy ''InputDispatch

-- | Create a new 'InputDispatch' object
mkInputDispatch :: MonadUnliftIO m => KeySource -> m InputDispatch
mkInputDispatch src = do
  (i, o) <- liftIO newChan
  a <- async $ liftIO . forever $ do
    e <- awaitKeyWith src
    writeChan i $ KIOEvent e
  link a
  InputDispatch i <$> newMVar o

-- | Inject an event into KMonad's app-loop
injectEvent :: HasInputDispatch e => Event -> RIO e ()
injectEvent e = view inChan >>= liftIO . (flip writeChan) e

-- | Wait for an event to occur and then return it
awaitEvent :: HasInputDispatch e => RIO e Event
awaitEvent = view outChan >>= \v -> withMVar v $ liftIO . readChan

-- | Get a copy of the event-stream
copyStream :: HasInputDispatch e => RIO e (OutChan Event)
copyStream = view inChan >>= liftIO . dupChan

-- | Capture the event-stream. This will block any and all processing of events
-- in KMonad until the release action is performed. This returns the channel
-- from which to read events, and the action to release the channel again.
--
-- Note: this does not stop any copies of the main stream from functioning. That
-- functionality is not supported.
--
captureStream :: HasInputDispatch e => RIO e (OutChan Event, RIO e ())
captureStream = do
  oV <- view outChan
  o  <- takeMVar oV
  pure $ (o, putMVar oV o)
