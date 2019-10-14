{-|
Module      : KMonad.Domain.Button
Description : Entrypoint for all effectful Button functionality
Copyright   : (c) David Janssen, 2019
License     : MIT

Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : non-portable (MPTC with FD, FFI to Linux-only c-code)

This module reexports all of the different 'Button' implementations from the
"KMonad.Domain.Button" submodule, and provides a function that translates a
'KMonad.Core.Parser.Token.ButtonToken' to a concrete implementation. For
anything that uses 'Button's, it should be sufficient to simply import this
module.

-}
module KMonad.Domain.Button
  ( encode

  , module KMonad.Domain.Button.After
  , module KMonad.Domain.Button.Around
  , module KMonad.Domain.Button.Block
  , module KMonad.Domain.Button.Emit
  , module KMonad.Domain.Button.EmitDeadKey
  , module KMonad.Domain.Button.EmitSpecial
  , module KMonad.Domain.Button.LayerAdd
  , module KMonad.Domain.Button.LayerRem
  , module KMonad.Domain.Button.LayerToggle
  , module KMonad.Domain.Button.Lockers
  , module KMonad.Domain.Button.Macro
  , module KMonad.Domain.Button.MultiTap
  , module KMonad.Domain.Button.TapHold
  , module KMonad.Domain.Button.TapNext
  )
where

import Control.Monad.Trans

import KMonad.Core.Button
import KMonad.Core.Parser
import KMonad.Domain.Effect (CanButton)


import KMonad.Domain.Button.After
import KMonad.Domain.Button.Around
import KMonad.Domain.Button.Block
import KMonad.Domain.Button.Emit
import KMonad.Domain.Button.EmitDeadKey
import KMonad.Domain.Button.EmitSpecial
import KMonad.Domain.Button.LayerAdd
import KMonad.Domain.Button.LayerRem
import KMonad.Domain.Button.LayerToggle
import KMonad.Domain.Button.Lockers
import KMonad.Domain.Button.Macro
import KMonad.Domain.Button.MultiTap
import KMonad.Domain.Button.TapHold
import KMonad.Domain.Button.TapNext


-- | Turn a ButtonToken into a Button operation
encode :: (CanButton m, MonadIO n) => ButtonToken -> n (Button m)
encode (BAfter a b) = mkAfter <$> encode a <*> encode b
encode (BEmit kc)     = mkEmitM kc
encode (BEmitSpecial ss) = mkEmitSpecialM ss
encode (BEmitDeadKey dk) = mkEmitDeadKeyM dk
encode (BModded kc b) = do
  x <- mkEmitM kc
  y <- encode  b
  mkAroundM x y
encode BBlock = mkBlockM
encode (BLayerToggle lid) = mkLayerToggleM lid
encode (BLayerAdd lid) = mkLayerAddM lid
encode (BLayerRem lid) = mkLayerRemM lid
encode (BTapHold ms bt bh) = do
  btap <- encode bt
  bhld <- encode bh
  mkTapHold ms btap bhld
encode (BTapNext bt bh) = do
  btap <- encode bt
  bhld <- encode bh
  mkTapNext btap bhld
encode (BMacro bs) = mkMacroM bs
encode (BMultiTap bs) = mkMultiTapM =<< mapM (\(t, b) -> (t,) <$> encode b) bs
encode (BLockOn lk) = mkLockOnM lk
encode (BLockOff lk) = mkLockOffM lk
encode (BLockToggle lk) = mkLockToggleM lk
