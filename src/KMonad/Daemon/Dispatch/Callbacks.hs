module KMonad.Daemon.Dispatch.Callbacks

where

import Prelude

-- | A collection of callbacks
data Registry a = Registry
  { _reg :: MVar [Callback a]
  }
makeLenses ''Registry

-- | Create a new, empty registry
mkRegistry :: MonadUnliftIO m => m (Registry a)
mkRegistry = Registry <$> newMVar []

-- | Run an event against all the callbacks in the registry, removing any
-- callback that matches and performing its action. If any callback matches,
-- return Nothing, else return 'Just' the event.
check :: MonadUnliftIO m => Registry a -> a -> m (Maybe a)
check r a = modifyMVar (r^.reg) $ \fs -> case match fs a of
  (new, []) -> pure (new, Just a)
  (new, io) -> liftIO (sequence_ io) >> pure (new, Nothing)

-- | Register a new callback
register :: MonadUnliftIO m => Registry a -> Callback a -> m ()
register r c = modifyMVar_ (r^.reg) $ pure . (c:)
