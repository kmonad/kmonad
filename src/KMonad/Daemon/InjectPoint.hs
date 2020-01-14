module KMonad.Daemon.InjectPoint
  ( InjectPoint
  , mkInjectPoint
  , HasInjectPoint
  , injectPoint
  , pull
  , inject
  )
where
-- FIXME: Clean and comment me plz
import Prelude

import KMonad.Keyboard
import KMonad.Event

data InjectPoint = InjectPoint
  { _pullSrc :: IO KeyEvent
  , _reading :: TVar Bool
  , _injectP :: TMVar (Either KeyEvent Event)
  }
makeClassy ''InjectPoint

mkInjectPoint :: RIO e KeyEvent -> RIO e InjectPoint
mkInjectPoint src = do
  u   <- askRunInIO
  rdn <- atomically $ newTVar False
  inj <- atomically $ newEmptyTMVar
  pure $ InjectPoint (u src) rdn inj

pull :: HasInjectPoint e => RIO e Event
pull = do
  -- Get a reference to the environment
  i <- view injectPoint

  -- Unless we are reading, start a thread that copies 1 event from src to chan
  r <- atomically . readTVar $ i^.reading
  unless r . void . async $ do
    e <- liftIO $ i^.pullSrc
    atomically . putTMVar (i^.injectP) $ Left e

  -- Read an event from the injection var, if it is a key-event, that means we
  -- are no longer reading, so we set reading to false. Either way, we return
  -- the event.
  atomically (takeTMVar $ i^.injectP) >>= \case
    Left ke -> do
      atomically $ writeTVar (i^.reading) False
      pure $ KeyIOEvent ke
    Right e -> pure e

-- | Inject an event into the event-loop
inject :: HasInjectPoint e => Event -> RIO e ()
inject e = view injectP >>= \ip -> atomically $ putTMVar ip (Right e)
