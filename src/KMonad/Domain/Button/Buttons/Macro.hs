{-|
Module      : KMonad.Domain.Button.Buttons.Macro
Description : A button that emits a series of events on press
Copyright   : (c) David Janssen, 2019
License     : MIT

Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : non-portable (MPTC with FD, FFI to Linux-only c-code)

A button that emits a sequence of keypresses when pressed

-}
module KMonad.Domain.Button.Buttons.Macro
  ( mkMacro
  )
where

import Control.Monad.IO.Class
import KMonad.Core
import KMonad.Domain.Effect
import KMonad.Domain.Button.Button

-- | Return a button that emits a series of events upon press
mkMacro :: (MonadIO io, MonadEmit m)
  => KeySequence   -- ^ The `KeySequence` to emit
  -> io (Button m) -- ^ The resulting button
mkMacro es = onPress $ emitSeq es
