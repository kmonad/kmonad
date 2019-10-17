{-|
Module      : KMonad.Domain.Button.Buttons.Macro
Description : A button that emits a series of events on press
Copyright   : (c) David Janssen, 2019
License     : MIT

Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : non-portable (MPTC with FD, FFI to Linux-only c-code)

-}
module KMonad.Domain.Button.Buttons.Macro
  ( mkMacro
  , mkMacroM
  )
where

import KMonad.Core
import KMonad.Domain.Effect

-- | Return a button that emits a series of events upon press
mkMacro :: MonadEmit m => KeySequence -> Button m
mkMacro es = mkButton $ \case
  Engaged   -> emitSeq es
  Disengaged -> pure ()

-- | Return a button that emits a series of events upon press from some arbitrary Monad
mkMacroM :: (Monad n, MonadEmit m) => KeySequence -> n (Button m)
mkMacroM = pure . mkMacro
