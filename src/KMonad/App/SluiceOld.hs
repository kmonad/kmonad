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

import KPrelude

import KMonad.Keyboard

-- | The 'Sluice' environment.
data Sluice = Sluice
  { _blocked  :: IORef Int
  , _blockBuf :: IORef [KeyEvent]
  }
makeClassy ''Sluice

-- | Create a new 'Sluice' environment
mkSluice' :: RIO e Sluice
mkSluice' = do
  bld <- newIORef 0
  buf <- newIORef []
  pure $ Sluice bld buf

mkSluice :: ContT r (RIO e) Sluice
mkSluice = lift mkSluice'

debugReport :: HasSluice e => RIO e Text
debugReport = do
  es <- map ((" - " <>) . textDisplay) <$> (readIORef =<< view blockBuf)
  pure . unlines $ "Storing event, current store: ":es

-- | Try to read from the Sluice, if we are blocked, store the event internally
-- and return Nothing. If we are unblocked, return Just the KeyEvent.
step :: (HasLogFunc e, HasSluice e) => RIO e KeyEvent -> RIO e (Maybe KeyEvent)
step pullSrc = do
  s <- view sluice
  e <- pullSrc
  readIORef (s^.blocked) >>= \case
    0 -> pure $ Just e
    _ -> do
      modifyIORef' (s^.blockBuf) (e:)
      logDebug . display =<< debugReport
      pure Nothing

-- | Keep trying to read from the Sluice until an event passes through
pull :: (HasLogFunc e, HasSluice e) => RIO e KeyEvent -> RIO e KeyEvent
pull pullSrc = step pullSrc >>= maybe (pull pullSrc) pure

blockReport :: (HasSluice e) => RIO e Utf8Builder
blockReport = do
  n <- readIORef =<< view blocked
  pure $ "Block level set to: " <> display n

-- | Set the Sluice to blocked mode.
block :: (HasSluice e, HasLogFunc e) => RIO e ()
block = do
  view blocked >>= \b -> modifyIORef b (+1)
  logDebug =<< blockReport

-- | Set the Sluice to unblocked mode, return a list of all the stored events
-- that should be rerun, in the correct order (head was first-in, etc).
unblock :: (HasSluice e, HasLogFunc e) => RIO e [KeyEvent]
unblock = do
  s <- view sluice
  modifyIORef' (s^.blocked) (\n -> n - 1)
  readIORef (s^.blocked) >>= \case
    0 -> do
      es <- readIORef (s^.blockBuf)
      writeIORef (s^.blockBuf) []
      logDebug $ "Unblocking input stream, " <>
        if null es
        then "no stored events"
        else "rerunning:\n" <> (display . unlines . map textDisplay $ reverse es)
      pure $ reverse es
    _ -> do
      logDebug =<< blockReport
      pure []
