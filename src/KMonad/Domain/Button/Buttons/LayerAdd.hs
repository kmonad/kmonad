{-|
Module      : KMonad.Domain.Button.Buttons.LayerAdd
Description : A button that adds a layer to the top of the stack
Copyright   : (c) David Janssen, 2019
License     : MIT

Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : non-portable (MPTC with FD, FFI to Linux-only c-code)

Permanently add a layer to the top of the layer stack

-}
module KMonad.Domain.Button.Buttons.LayerAdd
  ( mkLayerAdd
  , mkLayerAddM
  )
where

import KMonad.Core
import KMonad.Domain.Effect

mkLayerAdd :: (MonadTrace m, MonadStackManip m)
  => Name -- ^ The ID of the layer to add to the stack
  -> Button m
mkLayerAdd lid = mkButton $ \case
  Engaged   -> trace ("pushing layer: " <> lid) >> pushL lid
  Disengaged -> pure ()

mkLayerAddM :: (MonadTrace m, MonadStackManip m, Monad n)
  => Name -- ^ The ID of the layer to remove from the stack
  -> n (Button m)
mkLayerAddM = pure . mkLayerAdd
