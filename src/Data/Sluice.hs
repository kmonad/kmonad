module Data.Sluice
  ( -- * The Sluice datatype and its operations
    -- $slc
    Sluice
  , mkSluice
  , write
  , read
  , block
  , unblock
  , isBlocked
  )
where

import Prelude

import Control.Concurrent.Chan.Unagi


--------------------------------------------------------------------------------
-- $slc
--
-- A 'Sluice' is a type of multiprocessing channel with the added functionality
-- that its reading can be blocked and unblocked, allowing subsequent
-- computations to be paused and unpaused.

-- | The 'Sluice' type that functions as a pausable channel for some type `a`.
data Sluice a = Sluice
  { _chIn  :: !(InChan a)
  , _chOut :: !(OutChan a)
  , _sema  :: !(MVar ())
  }
makeLenses ''Sluice

-- | Create a new 'Sluice' in its default empty, unblocked state.
mkSluice :: MonadUnliftIO m => m (Sluice a)
mkSluice = do
  (i, o) <- liftIO newChan
  Sluice i o <$> newMVar ()

-- | Write a thing to the 'Sluice'
write :: MonadUnliftIO m => a -> Sluice a -> m ()
write e = liftIO . flip writeChan e . view chIn

-- | Read a thing from the 'Sluice', this will block if the 'Sluice' is empty or
-- in blocked mode.
read :: MonadUnliftIO m => Sluice a -> m a
read sl = withMVar (sl^.sema) $ \_ -> liftIO . readChan $ (sl^.chOut)

-- | Block a 'Sluice', blocking future calls until unblocked. This call itself
-- will block if the 'Sluice' is already blocked.
block :: MonadUnliftIO m => Sluice a -> m ()
block s = do
  traceIO $ "trying to take"
  takeMVar . view sema $ s
  traceIO $ "I took"

-- | Unblock a 'Sluice', allowing any readers to continue reading. This call
-- itself will block if the 'Sluice' is already unblocked.
unblock :: MonadUnliftIO m => Sluice a -> m ()
unblock s = do
  traceIO $ "trying to put"
  flip putMVar () . view sema $ s
  traceIO $ "I put"


-- | Check whether the 'Sluice' is in blocked mode.
isBlocked :: MonadUnliftIO m => Sluice a -> m Bool
isBlocked = isEmptyMVar . view sema
