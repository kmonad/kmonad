module KMonad.Model.EventSrc
  ( EventSrc(..)
  , unliftESrc
  , pullESrc
  , pullToESrc
  , toESrc
  ) where

import KMonad.Prelude
import KMonad.Util
import KMonad.Keyboard

-- | Used when a component needs an event source.
-- The benefit of this over spawning a new thread and awaiting the async option are the following:
-- 1. No making sure the event doesn't get dropped
-- 2. No key events blocked inside the pipeline (see #1048 fixed by #1049)
-- Instead it needs to construct an STM action including those of the event source.
-- Due to the existentially qualified nature, we can only work with it via
-- pattern matching inside a function argument.
data EventSrc m = forall a. EventSrc
  { tryESrc :: STM a
  , postESrc :: a -> m (Maybe KeyEvent)
  }

unliftESrc :: (m (Maybe KeyEvent) -> IO (Maybe KeyEvent)) -> EventSrc m -> EventSrc IO
unliftESrc u EventSrc{tryESrc, postESrc} = EventSrc tryESrc (u . postESrc)

pullESrc :: MonadIO m => EventSrc m -> m KeyEvent
pullESrc EventSrc{tryESrc, postESrc} = go
 where go = atomically tryESrc >>= postESrc >>= maybe go pure

-- | Spawns a new thread to continously pull
pullToESrc :: (HasLogFunc e, MonadIO m) => Text -> RIO e KeyEvent -> ContT r (RIO e) (EventSrc m)
pullToESrc tname pull = do
  channel <- newEmptyTMVarIO
  launch_ tname $ do
    e <- pull
    atomically $ putTMVar channel e

  pure $ toESrc channel

toESrc :: MonadIO m => TMVar KeyEvent -> EventSrc m
toESrc channel = EventSrc (takeTMVar channel) (pure . Just)
