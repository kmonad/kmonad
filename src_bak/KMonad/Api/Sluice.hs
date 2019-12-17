{-|
Module      : KMonad.Api.Sluice
Description : An implementation of pausable computation
Copyright   : (c) David Janssen, 2019
License     : MIT
Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : portable

This module implements a datatype 'Sluice' that does one of two things based on
its state. When open, it lets an action pass right through, at which point the
action will be performed. When closed it simply stores the action internally.
Once opened again, all of the actions that are stored up are sequenced and
returned in original order.

The output of the actions that are being sluiced must be monoidal, since we have
to be able to return mempty when closed. Additionally, we mappend all the
results when reopening a sluice.

-}
module KMonad.Api.Sluice
  ( Sluice
  , HasSluice
  , sluice
  , mkSluice
  , feed
  , openSluice
  , closeSluice
  )
where

import Control.Lens
import Control.Monad.Trans
import UnliftIO.MVar

-- | The type of sluice, parametrized on the effect, and the output
data Sluice m a = Sluice
  { _queue  :: MVar [m a]
  , _isOpen :: MVar Bool
  }

-- | Classy lens to express the concept of having a sluice
makeClassy ''Sluice

type CanSluice m a = (MonadIO m, Monoid a)

-- | Create a new sluice in its open and empty state
mkSluice :: MonadIO n => n (Sluice m a)
mkSluice = Sluice <$> newMVar [] <*> newMVar True

-- | Given a sluice, choose between two actions based on the sluice's state
withSluice :: CanSluice m a
  => Sluice m a -- ^ The sluice to operate on
  -> m a        -- ^ The action to perform if open
  -> m a        -- ^ The action to perform if closed
  -> m a
withSluice s a b = (readMVar $ s^.isOpen) >>= \p -> if p then a else b

-- | If the sluice is open, return the action, otherwise store it
feed :: CanSluice m a
  => m a        -- ^ The action to potentially perform
  -> Sluice m a -- ^ The sluice used to manage it
  -> m a        -- ^ Either the action or pure mempty
feed a s = withSluice s a $ do
  liftIO $ modifyMVar_ (s^.queue) (pure . ([a] <>))
  return mempty

-- | Set the sluice to open. If the sluice is already open, this returns pure mempty
openSluice :: CanSluice m a
  => Sluice m a -- ^ The Sluice to open
  -> m a        -- ^ An action containing the mappended results from all stored actions
openSluice s = withSluice s (return mempty) $ do
  _  <- swapMVar (s^.isOpen) True
  ls <- swapMVar (s^.queue) []
  mconcat <$> (sequence $ reverse ls)

-- | Set the sluice to closed, this always returns pure mempty
closeSluice :: CanSluice m a
  => Sluice m a -- ^ The Sluice to close
  -> m a        -- ^ Monadic mempty
closeSluice s = swapMVar (s^.isOpen) False >> return mempty

