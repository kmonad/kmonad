{-|
Module      : KMonad.Domain.Button.Buttons.EmitDeadKey
Description : A button that emits deadkey encoding macros
Copyright   : (c) David Janssen, 2019
License     : MIT

Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : non-portable (MPTC with FD, FFI to Linux-only c-code)

A button that emits macros mapped to dead-keys.

-}
module KMonad.Domain.Button.Buttons.EmitDeadKey
  ( mkEmitDeadKey
  )
where

import Control.Monad.IO.Class
import KMonad.Core
import KMonad.Domain.Effect
import KMonad.Domain.Button.Button

-- | Return a button that emits a special symbol on Press (does nothing on Release)
mkEmitDeadKey :: (MonadIO io, MonadSymbol m)
  => DeadKey       -- ^ The DeadKey to emit
  -> io (Button m) -- ^ The resulting button
mkEmitDeadKey dk = mkButton $ \case
  Engaged    -> emitDeadKey dk
  Disengaged -> pure ()

