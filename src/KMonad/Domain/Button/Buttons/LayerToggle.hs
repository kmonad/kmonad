{-|
Module      : KMonad.Domain.Button.Buttons.LayerToggle
Description : A button that toggles a layer on and off
Copyright   : (c) David Janssen, 2019
License     : MIT

Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : non-portable (MPTC with FD, FFI to Linux-only c-code)

Temporarily switch to a layer on press, and switch back on release.

-}
module KMonad.Domain.Button.Buttons.LayerToggle
  ( mkLayerToggle
  )
where

import Control.Lens
import Control.Monad.IO.Class
import KMonad.Core
import KMonad.Domain.Effect
import KMonad.Domain.Button.Button

-- | Return a 'Button' that pushes a layer onto the stack when pressed, and pops
-- it when released.
mkLayerToggle :: (MonadIO io, MonadLogger m, MonadStackManip m)
  => Name          -- ^ The ID of the layer to toggle to
  -> io (Button m) -- ^ The resulting button
mkLayerToggle lid = mkButton $ \case
  Engaged    -> pushL lid
  Disengaged -> popL  lid
