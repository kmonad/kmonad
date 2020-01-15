module KMonad.Daemon.Sluice
  ( Sluice
  , mkSluice
  , HasSluice
  , sluice
  , pull
  , block
  , unblock
  )
where

import Prelude

import KMonad.Keyboard

-- | The 'Sluice' environment.
data Sluice = Sluice
  { _pullSrc  :: IO KeyEvent
  , _blocked  :: TVar Bool
  , _blockBuf :: TVar [KeyEvent]
  }
makeClassy ''Sluice

-- | Create a new 'Sluice' environment
mkSluice' :: RIO e KeyEvent -> RIO e Sluice
mkSluice' src = withRunInIO $ \u -> do
  bld <- atomically $ newTVar False
  buf <- atomically $ newTVar []
  pure $ Sluice (u src) bld buf

mkSluice :: RIO e KeyEvent -> ContT r (RIO e) Sluice
mkSluice = lift . mkSluice'

-- | Try to read from the Sluice, if we are blocked, store the event internally
-- and return Nothing. If we are unblocked, return Just the KeyEvent.
step :: HasSluice e => RIO e (Maybe KeyEvent)
step = do
  s <- view sluice
  e <- liftIO =<< view pullSrc
  atomically $ readTVar (s^.blocked) >>= \case
    True  -> modifyTVar (s^.blockBuf) (e:) >> pure Nothing
    False -> pure $ Just e

-- | Keep trying to read from the Sluice until an event passes through
pull :: HasSluice e => RIO e KeyEvent
pull = step >>= maybe pull pure

-- | Set the Sluice to blocked mode.
block :: HasSluice e => RIO e ()
block = view blocked >>= \b -> atomically $ writeTVar b True

-- | Set the Sluice to unblocked mode, return a list of all the stored events
-- that should be rerun, in the correct order (head was first-in, etc).
unblock :: HasSluice e => RIO e [KeyEvent]
unblock = do
  s <- view sluice
  es <- atomically $ do
    writeTVar (s^.blocked) False
    swapTVar (s^.blockBuf) []
  pure $ reverse es
