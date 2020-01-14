module KMonad.Daemon.Dispatch.CChan
  ( -- * The 'CChan' channel implementation
    -- $cchan
    CChan
  , mkCChan
  , write
  , read
  , copy
  , capture
  , release
  )
where

import Prelude

import Control.Concurrent.Chan.Unagi


data CChan a = CChan
  { _chIn     :: InChan a
  , _chOut    :: OutChan a
  , _capPoint :: MVar (a -> IO ())
  }
makeLenses ''CChan

-- | Return a new 'CChan'
mkCChan :: MonadUnliftIO m => m (CChan a)
mkCChan = do
  (i, o)   <- liftIO newChan
  sf       <- newEmptyMVar
  pure $ CChan i o sf

-- | Write a value into the front of the CChan
write :: MonadUnliftIO m => CChan a -> a -> m ()
write c a = liftIO $ writeChan (c^.chIn) a

-- | Read a value. If the stream is captured this will block until the stream is
-- released again.
read :: MonadUnliftIO m => CChan a -> m a
read c = do
  e <- liftIO . readChan $ c^.chOut
  tryReadMVar (c^.capPoint) >>= \case
    Nothing -> pure e
    Just f  -> do
      liftIO $ f e
      read c

-- | Register a callback to run on input instead of returning
capture :: MonadUnliftIO m => CChan a -> (a -> m ()) -> m ()
capture c f = withRunInIO $ \u -> putMVar (c^.capPoint) (u . f)

-- | Acquire a copy of the input stream
copy :: MonadUnliftIO m => CChan a -> m (m a)
copy c = (liftIO . readChan) <$> liftIO (dupChan $ c^.chIn)

-- | Unregister the capture of input
release :: MonadUnliftIO m => CChan a -> m ()
release = void . takeMVar . view capPoint
