{-|
Module      : KMonad.App.Sluice
Description : The component that provides pausing functionality
Copyright   : (c) David Janssen, 2019
License     : MIT
Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : portable

For certain KMonad operations we need to be able to pause and resume processing
of events. This component provides the ability to temporarily pause processing,
and then resume processing and return all events that were caught while paused.

-}
module KMonad.App.Sluice
  ( Sluice
  , mkSluice
  , block
  , unblock
  , pull
  )
where

import KMonad.Prelude

import KMonad.Keyboard

--------------------------------------------------------------------------------
-- $env

-- | The 'Sluice' environment.
--
-- NOTE: 'Sluice' has no internal multithreading, i.e. its 'pull' action will
-- never be interrupted, therefore we can simply use 'IORef' and sidestep all
-- the STM complications.
data Sluice = Sluice
  { _eventSrc :: IO KeyEvent      -- ^ Where we get our 'KeyEvent's from
  , _blocked  :: IORef Int        -- ^ How many locks have been applied to the sluice
  , _blockBuf :: IORef [KeyEvent] -- ^ Internal buffer to store events while closed
  }
makeLenses ''Sluice

-- | Create a new 'Sluice' environment
mkSluice' :: MonadUnliftIO m => m KeyEvent -> m Sluice
mkSluice' s = withRunInIO $ \u -> do
  bld <- newIORef 0
  buf <- newIORef []
  pure $ Sluice (u s) bld buf

-- | Create a new 'Sluice' environment, but do so in a ContT context
mkSluice :: MonadUnliftIO m => m KeyEvent -> ContT r m Sluice
mkSluice = lift . mkSluice'


--------------------------------------------------------------------------------
-- $op
--
-- The following code deals with simple operations on the environment, like
-- blocking and unblocking the sluice.

-- | Increase the block-count by 1
block :: HasLogFunc e => Sluice -> RIO e ()
block s = do
  modifyIORef (s^.blocked) (+1)
  readIORef (s^.blocked) >>= \n ->
    logDebug $ "Block level set to: " <> display n

-- | Set the Sluice to unblocked mode, return a list of all the stored events
-- that should be rerun, in the correct order (head was first-in, etc).
--
-- NOTE: After successfully unblocking the 'Sluice' will be empty, it is the
-- caller's responsibility to insert the returned events at an appropriate
-- location in the 'KMonad.App.App'.
--
-- We do this in KMonad by writing the events into the
-- 'KMonad.App.Dispatch.Dispatch's rerun buffer. (this happens in the
-- "KMonad.App" module.)
unblock :: HasLogFunc e => Sluice -> RIO e [KeyEvent]
unblock s = do
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
    n -> do
      logDebug $ "Block level set to: " <> display n
      pure []


--------------------------------------------------------------------------------
-- $loop
--
-- The following code deals with how a 'Sluice' fits into the KMonad pull-chain.
-- As long as we are blocked, we do not return any events, but keep storing them
-- internally. When we are unblocked, events simply pass through.


-- | Try to read from the Sluice, if we are blocked, store the event internally
-- and return Nothing. If we are unblocked, return Just the KeyEvent.
step :: HasLogFunc e => Sluice -> RIO e (Maybe KeyEvent)
step s = do
  e <- liftIO $ s^.eventSrc
  readIORef (s^.blocked) >>= \case
    0 -> pure $ Just e
    _ -> do
      modifyIORef' (s^.blockBuf) (e:)
      readIORef (s^.blockBuf) >>= \es -> do
        let xs = map ((" - " <>) . textDisplay) es
        logDebug . display . unlines $ "Storing event, current store: ":xs
      pure Nothing

-- | Keep trying to read from the Sluice until an event passes through
pull :: HasLogFunc e => Sluice -> RIO e KeyEvent
pull s = step s >>= maybe (pull s) pure
