module KMonad.Daemon.InjectPoint
  ( InjectPoint
  , mkInjectPoint
  , HasInjectPoint
  , injectPoint
  , pull
  , inject
  )
where

import KPrelude

import KMonad.Keyboard
import KMonad.Event

--------------------------------------------------------------------------------
-- $env
--

data InjectPoint = InjectPoint
  { _reading :: TVar Bool
  , _injectP :: TMVar (Either KeyEvent Event)
  }
makeClassy ''InjectPoint

mkInjectPoint' :: RIO e InjectPoint
mkInjectPoint' = do
  rdn <- atomically $ newTVar False
  inj <- atomically $ newEmptyTMVar
  pure $ InjectPoint rdn inj

mkInjectPoint ::ContT r (RIO e) InjectPoint
mkInjectPoint = lift mkInjectPoint'

--------------------------------------------------------------------------------
-- $loop

pull :: (HasLogFunc e, HasInjectPoint e)
  => RIO e KeyEvent
  -> RIO e Event
pull pullSrc = do
  -- Get a reference to the environment
  i <- view injectPoint

  -- Unless we are reading, start a thread that copies 1 event from src to chan
  r <- atomically . readTVar $ i^.reading
  unless r . void . async $ do
    e <- pullSrc
    atomically . putTMVar (i^.injectP) $ Left e

  -- Read an event from the injection var, if it is a key-event, that means we
  -- are no longer reading, so we set reading to false. Either way, we return
  -- the event.
  atomically (takeTMVar $ i^.injectP) >>= \case
    Left ke -> do
      atomically $ writeTVar (i^.reading) False
      pure $ KeyIOEvent ke
    Right e -> pure e


--------------------------------------------------------------------------------
-- $ops

-- | Inject an event into the event-loop
inject :: HasInjectPoint e => Event -> RIO e ()
inject e = view injectP >>= \ip -> atomically $ putTMVar ip (Right e)
