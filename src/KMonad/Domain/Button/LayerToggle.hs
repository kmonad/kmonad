{-|
Module      : KMonad.Domain.Button.LayerToggle
Description : A button that toggles a layer on and off
Copyright   : (c) David Janssen, 2019
License     : MIT

Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : non-portable (MPTC with FD, FFI to Linux-only c-code)

This is a rather naive implementation of a layer toggling button that simply
pushes a layer onto the layer stack when pressed, and pops it when released. It
is naive, because there is currently nothing stopping anyone from defining
configuration where a layer-toggle overlays itself and thereby makes it
impossible to pop the layer again.

Perhaps I can fix this with a 'pinComparison'.

Also, currently, this button does not obey the button-law.

-}
module KMonad.Domain.Button.LayerToggle
  ( mkLayerToggle
  , mkLayerToggleM
  )
where


import KMonad.Core
import KMonad.Domain.Effect

-- | Return a 'Button' that pushes a layer onto the stack when pressed, and pops
-- it when released.
mkLayerToggle :: (MonadTrace m, MonadStackManip m) => LayerId -> Button m
mkLayerToggle lid = mkButton $ \case
  BPress   -> trace ("pushing layer: " <> lid) >> pushL lid
  BRelease -> trace ("popping layer: " <> lid) >> popL lid

-- | Return a LayerToggle from some arbitrary Monad
mkLayerToggleM :: (MonadTrace m, MonadStackManip m, Monad n) => LayerId -> n (Button m)
mkLayerToggleM = return . mkLayerToggle
