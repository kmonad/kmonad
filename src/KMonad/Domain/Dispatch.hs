module KMonad.Domain.Dispatch
  ( Dispatch
  , mkDispatch
  , injectEvent
  , awaitEvent
  , copyStream
  , captureStream
  )
where

import KMonad.Prelude

import Control.Concurrent.Chan.Unagi

import KMonad.Domain.KeyIO
import KMonad.Domain.Types

data Dispatch = Dispatch
  { _inChan  ::  InChan Event
  , _outChan :: MVar (OutChan Event)
  }
makeClassy ''Dispatch

-- | Create a new 'Dispatch' object
mkDispatch :: MonadUnliftIO m => KeySource -> m Dispatch
mkDispatch src = do
  (i, o) <- liftIO newChan
  a <- async $ pipeEvents i
  link a
  Dispatch i <$> newMVar o

  where
    -- Forever copy key-events into the input channel
    pipeEvents i = liftIO . forever $ do
      e <- awaitKeyWith src
      writeChan i $ InputEvent e

-- | Write an event to the 'Dispatch' object
injectEvent :: HasDispatch e => Event -> RIO e ()
injectEvent e = view inChan >>= liftIO . (flip writeChan) e

-- | Wait for the next 'Event' to occur and return it
awaitEvent :: HasDispatch e => RIO e Event
awaitEvent = view outChan >>= \v -> withMVar v $ liftIO . readChan

-- | Get a copy of the event-stream
copyStream :: HasDispatch e => RIO e (OutChan Event)
copyStream = view inChan >>= liftIO . dupChan

-- | Capture the event-stream. This will block any and all processing of events
-- in KMonad until the release action is performed. This returns the channel
-- from which to read events, and the action to release the channel again.
--
-- Note: this does not stop any copies of the main stream from functioning. That
-- functionality is not supported at all in KMonad.
--
captureStream :: HasDispatch e => RIO e (OutChan Event, RIO e ())
captureStream = do
  oV <- view outChan
  o  <- takeMVar oV
  pure $ (o, putMVar oV o)
