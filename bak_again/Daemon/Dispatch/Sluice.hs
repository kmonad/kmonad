module KMonad.Daemon.Dispatch.Sluice

where

import Prelude

import qualified RIO.Seq as Seq

--------------------------------------------------------------------------------

-- | The 'Sluice' data type
data Sluice a = Sluice
  { _pull  :: IO a             -- ^ The action we use to pull events
  , _free  :: MVar ()          -- ^ Is Sluice open? full -> Y, empty -> N.
  , _store :: MVar (Seq.Seq a) -- ^ Buffer to store events when closed
  }
makeLenses ''Sluice

-- | Return a new, empty, open 'Sluice'
mkSluice :: MonadUnliftIO m => m a -> m (Sluice a)
mkSluice a = withRunInIO $ \u ->
  Sluice (u a) <$> newMVar () <*> newMVar Seq.empty


-- | Read an item from the pull-source
--
-- If we are not blocked, then we simply return the event. If we are blocked, we
-- put the event into our store and keep trying to pull. Eventually one of our
-- pulled actions will unblock us, which will then allow read to return
read :: MonadUnliftIO m => Sluice a -> m a
read sl = do
  a <- liftIO $ sl^.pull
  b <- isEmptyMVar $ sl^.free
  if b
    then do
      modifyMVar_ (sl^.store) (pure . (|> a))
      read sl
    else pure a

-- | Block the sluice
--
-- If the sluice is already blocked, this action will block.
block :: MonadUnliftIO m => Sluice a -> m ()
block = takeMVar . view free

-- | Unblock the sluice and return all the stored events
--
-- If the sluice is already unblocked, this action will block.
unblock :: MonadUnliftIO m => Sluice a -> m (Seq.Seq a)
unblock sl = do
  putMVar (sl^.free) ()
  swapMVar (sl^.store) Seq.empty
