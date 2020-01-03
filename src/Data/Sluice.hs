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

import KMonad.Util

--------------------------------------------------------------------------------
-- $slc
--
-- A 'Sluice' is a type of multiprocessing channel with the added functionality
-- that its reading can be blocked and unblocked, allowing subsequent
-- computations to be paused and unpaused.

-- | The 'Sluice' type that functions as a pausable channel for some type `a`.
data Sluice a = Sluice
  { _chIn  :: !(InChan a)
  , _out   :: !(MVar a)
  , _sema  :: !(MVar ())
  }
makeLenses ''Sluice

withSluice :: HasLogFunc e => (Sluice b -> RIO e a) -> RIO e a
withSluice f = do
  (i, o) <- liftIO newChan
  sem    <- newMVar ()
  mo     <- newEmptyMVar

  let copyOver = do
        e <- liftIO . readChan $ o
        withMVar sem $ \_ ->
          putMVar mo e

  withThread "sluice:internal" copyOver . f $
    Sluice i mo sem

-- | Create a new 'Sluice' in its default empty, unblocked state.
mkSluice :: HasLogFunc e => ContT r (RIO e) (Sluice a)
mkSluice = ContT withSluice

-- | Write a thing to the 'Sluice'
write :: MonadUnliftIO m => a -> Sluice a -> m ()
write e = liftIO . flip writeChan e . view chIn

-- | Read a thing from the 'Sluice', this will block if the 'Sluice' is empty or
-- in blocked mode.
read :: MonadUnliftIO m => Sluice a -> m a
read = takeMVar . view out

-- | Block a 'Sluice', blocking future calls until unblocked. This call itself
-- will block if the 'Sluice' is already blocked.
block :: MonadUnliftIO m => Sluice a -> m ()
block = takeMVar . view sema

-- | Unblock a 'Sluice', allowing any readers to continue reading. This call
-- itself will block if the 'Sluice' is already unblocked.
unblock :: MonadUnliftIO m => Sluice a -> m ()
unblock = flip putMVar () . view sema

-- | Check whether the 'Sluice' is in blocked mode.
isBlocked :: MonadUnliftIO m => Sluice a -> m Bool
isBlocked = isEmptyMVar . view sema
