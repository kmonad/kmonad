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
  )
where

import Control.Monad.IO.Class
import KMonad.Core
import KMonad.Domain.Effect
import KMonad.Domain.Button.Button

-- | Return a 'Button' that adds a layer to the stack when pressed.
mkLayerAdd :: (MonadIO io, MonadStackManip m)
  => Name          -- ^ The ID of the layer to add to the stack
  -> io (Button m) -- ^ The resulting button
mkLayerAdd lid = onPress $ pushL lid

