module KMonad.Domain.Effect.Effects.MonadSymbol
  ( MonadSymbol(..)
  )
where

import KMonad.Core
import KMonad.Domain.Effect.Effects.MonadEmit

-- | The 'MonadSymbol' effect, used to emit special characters using
-- key-sequences.

class  MonadEmit m => MonadSymbol m where
  emitSymbol  :: SpecialSymbol -> m ()
  emitDeadKey :: DeadKey -> m ()
