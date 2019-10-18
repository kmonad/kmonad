{-|
Module      : KMonad.Domain.Button.Buttons.EmitSpecial
Description : A button that emits special-symbol macros on press
Copyright   : (c) David Janssen, 2019
License     : MIT

Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : non-portable (MPTC with FD, FFI to Linux-only c-code)

A button that emits a macro for special symbols.

-}
module KMonad.Domain.Button.Buttons.EmitSpecial
  ( mkEmitSpecial
  )
where

import Control.Monad.IO.Class
import KMonad.Core
import KMonad.Domain.Effect
import KMonad.Domain.Button.Button

-- | Return a button that emits a special symbol on Press (does nothing on Release)
mkEmitSpecial :: (MonadIO io, MonadSymbol m)
  => SpecialSymbol -- ^ The SpecialSymbol to emit
  -> io (Button m) -- ^ The resulting button
mkEmitSpecial kc = onPress $ emitSymbol kc
