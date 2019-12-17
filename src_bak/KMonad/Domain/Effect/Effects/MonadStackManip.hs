module KMonad.Domain.Effect.Effects.MonadStackManip
  ( MonadStackManip(..)
  )
where

import KMonad.Core

-- | This effect allows for the manipulation of the stack of layers.
class Monad m => MonadStackManip m where
  pushL :: Name -> m ()
  popL  :: Name -> m ()
