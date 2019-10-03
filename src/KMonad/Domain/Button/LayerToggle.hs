{-|
Module      : KMonad.Domain.Button.LayerToggle
Description : A button that toggles a layer on and off
Copyright   : (c) David Janssen, 2019
License     : MIT

Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : non-portable (MPTC with FD, FFI to Linux-only c-code)

Temporarily switch to a layer on press, and switch back on release.

-}
module KMonad.Domain.Button.LayerToggle
  ( LTCtx
  , mkLayerToggle
  , mkLayerToggleM
  )
where

import KMonad.Core
import KMonad.Domain.Effect

-- | The context required by LayerToggle buttons
type LTCtx m =
  ( MonadTrace      m
  , MonadStackManip m
  )

-- | Return a 'Button' that pushes a layer onto the stack when pressed, and pops
-- it when released.
mkLayerToggle :: (LTCtx m) => Name -> Button m
mkLayerToggle lid = mkButton $ \case
  BPress   -> trace ("pushing layer: " <> lid) >> pushL lid
  BRelease -> trace ("popping layer: " <> lid) >> popL  lid

-- | Return a LayerToggle from some arbitrary Monad
mkLayerToggleM :: (LTCtx m, Monad n) => Name -> n (Button m)
mkLayerToggleM = return . mkLayerToggle
