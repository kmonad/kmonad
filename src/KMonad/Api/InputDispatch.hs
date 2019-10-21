module KMonad.Api.InputDispatch

where

import Control.Concurrent.Chan.Unagi
import Control.Lens
import Control.Monad (forever)
import Control.Monad.IO.Class
import UnliftIO.MVar
import KMonad.Core
import KMonad.Domain.Effect
import KMonad.Api.KeyIO

data InputDispatch = InputDispatch
  { _iCh :: InChan KeyEvent
  , _oCh :: MVar (OutChan KeyEvent)
  }
makeLenses ''InputDispatch

mkInputDispatch :: (MonadFork m, MonadIO m)
  => Receiver        -- ^ A blocking action that gets the next KeyEvent
  -> m InputDispatch -- ^ The device that dispatches inputs
mkInputDispatch a = do
  (i, o) <- liftIO newChan
  fork $ f i
  InputDispatch i <*> newMVar o
  where
    f inCh = liftIO . forever $ writeChan inCh =<< a


-- | Await the next event from an EventSource, this blocks until an event occurs
-- readEvent :: (MonadRace m, MonadIO m) => EventSource -> m Event
-- readEvent es =
--   race (InputEvent <$> es^.waitKey) (takeMVar $ es^.injectV) >>= \case
--     Left e  -> return e
--     Right e -> return e

-- | Inject a new event into the EventSource, can block if the events aren't
-- being handled fast enough.
writeEvent :: MonadIO m => InputDispatch -> Event -> m ()
writeEvent d e = liftIO . putMVar (d^.injectV) $ e

readEvent :: (MonadRace m, MonadIO m) => InputDispatch -> m Event
readEvent d = undefined
  -- race (InputEvent <$> d^)
