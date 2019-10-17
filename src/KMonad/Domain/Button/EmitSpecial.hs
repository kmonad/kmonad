{-|
Module      : KMonad.Domain.Button.EmitSpecial
Description : A button that emits special-symbol macros on press
Copyright   : (c) David Janssen, 2019
License     : MIT

Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : non-portable (MPTC with FD, FFI to Linux-only c-code)

-}
module KMonad.Domain.Button.EmitSpecial
  ( mkEmitSpecial
  , mkEmitSpecialM
  )
where

import KMonad.Core
import KMonad.Domain.Effect

-- | Return a button that emits a special symbol on Press (does nothing on Release)
mkEmitSpecial :: (MonadSymbol m)
  => SpecialSymbol
  -> Button m
mkEmitSpecial kc = mkButton $ \case
  Engaged   -> emitSymbol kc
  Disengaged -> pure ()

-- | Return a button that emits a mkEmitSpecial button from an arbitrary Monad
mkEmitSpecialM :: (MonadSymbol m, Monad n)
  => SpecialSymbol
  -> n (Button m)
mkEmitSpecialM = pure . mkEmitSpecial
