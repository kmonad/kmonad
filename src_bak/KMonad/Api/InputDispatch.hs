module KMonad.Api.InputDispatch

where

import Control.Concurrent (forkIO)
import Control.Concurrent.Chan.Unagi
import Control.Lens
import Control.Monad (forever)
import Control.Monad.IO.Class
import UnliftIO.MVar
import KMonad.Core
import KMonad.Api.KeyIO

data InputDispatch = InputDispatch
  { _iCh :: InChan Event
  , _oCh :: MVar (OutChan Event)
  }
makeClassy ''InputDispatch

-- TODO: make sure the reader-thread gets closed upon shutdown
mkInputDispatch :: MonadIO m
  => Receiver        -- ^ A blocking action that gets the next KeyEvent
  -> m InputDispatch -- ^ The device that dispatches inputs
mkInputDispatch nextKey = do
  (i, o) <- liftIO newChan
  _ <- liftIO . forkIO $ f i
  InputDispatch i <$> newMVar o
  where
    f inCh = liftIO . forever $ writeChan inCh =<< InputEvent <$> nextKey


-- | Await the next event from an EventSource, this blocks until an event occurs
-- readEvent :: (MonadRace m, MonadIO m) => EventSource -> m Event
-- readEvent es =
--   race (InputEvent <$> es^.waitKey) (takeMVar $ es^.injectV) >>= \case
--     Left e  -> return e
--     Right e -> return e

-- | Inject a new event into the EventSource, can block if the events aren't
-- being handled fast enough.
writeEvent :: MonadIO m => Event -> InputDispatch -> m ()
writeEvent e d = liftIO . writeChan (d^.iCh) $ e

-- | Read the next event to occur (blocking read)
readEvent :: MonadIO m => InputDispatch -> m Event
readEvent d = do
  o <- takeMVar $ d^.oCh
  v <- liftIO $ readChan o
  putMVar (d^.oCh) o
  pure v

-- | Return an action that returns the next event to occur, without capturing
-- from the normal handler. (All events get duplicated).
copyStream :: MonadIO m => InputDispatch -> m (m Event)
copyStream d = do
  o <- liftIO . dupChan $ d^.iCh
  pure . forever . liftIO $ readChan o

-- | Return an action that returns the next event to occur, and an action to
-- release the capture. Events will no longer be processed normally by KMonad
-- until the release action is called, at which point the the next-event action
-- should no longer be used. If KMonad is currently awaiting an input, this
-- action will block until that input is read, after which the grab will occur
-- (this only happens if the grab was launched asynchronously, if launched from
-- the main thread of a launcher, KMonad is guaranteed to no longer be in
-- /waiting/ mode).
captureStream :: MonadIO m => InputDispatch -> m (m Event, m ())
captureStream d = do
  o <- takeMVar $ d^.oCh
  pure $ (liftIO $ readChan o, putMVar (d^.oCh) o)
