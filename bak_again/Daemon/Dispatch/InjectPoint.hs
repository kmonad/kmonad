module KMonad.Daemon.Dispatch.InjectPoint
  ( InjectPoint
  , mkInjectPoint
  , read
  , inject
  )
where

import Prelude

import GHC.Conc (orElse)

data InjectPoint a = InjectPoint
  { _tick    :: IO a
  , _injtTgt :: TChan a
  , _tickTgt :: TMVar a
  , _ticking :: TMVar ()
  }
makeLenses ''InjectPoint

mkInjectPoint :: MonadUnliftIO m => m a -> m (InjectPoint a)
mkInjectPoint t = withRunInIO $ \u -> do
  i  <- atomically newTChan
  tt <- atomically newEmptyTMVar
  tn <- atomically newEmptyTMVar
  pure $ InjectPoint (u t) i tt tn

ensureTicking :: MonadUnliftIO m => InjectPoint a -> m ()
ensureTicking i = do
  join . atomically $ do
    isEmptyTMVar (i^.ticking) >>= \case
      False -> pure (pure ())
      True  -> do
        putTMVar (i^.ticking) ()
        pure startTick
  where
    startTick = void . async $ (liftIO $ i^.tick)
      >>= atomically . putTMVar (i^.tickTgt)

read :: MonadUnliftIO m => InjectPoint a -> m a
read i = do
  -- First try just reading an injected event
  atomically (tryReadTChan $ i^.injtTgt) >>= \case
    Just e -> pure e
    Nothing -> do
      -- Make sure we are running a tick thread
      ensureTicking i
      atomically $ (readTChan $ i^.injtTgt)
          `orElse` (takeTMVar $ i^.tickTgt)

inject :: MonadUnliftIO m => InjectPoint a -> a -> m ()
inject i = atomically . writeTChan (i^.injtTgt)
