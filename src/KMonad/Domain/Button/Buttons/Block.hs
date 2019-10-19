{-|
Module      : KMonad.Domain.Button.Buttons.Block
Description : A button that captures the Event but does nothing
Copyright   : (c) David Janssen, 2019
License     : MIT

Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : non-portable (MPTC with FD, FFI to Linux-only c-code)

When a 'KMonad.Core.KeyCode.KeyCode' does not occur in a layer, it is passed
down the stack into further layers, so if you want to block a key explicitly,
include it in the layer, but define it as a 'mkBlock' 'Button'.

-}
module KMonad.Domain.Button.Buttons.Block
  ( mkBlock
  )
where

import Control.Monad.IO.Class
import KMonad.Domain.Button.Button

-- | Create a Button that does nothing
mkBlock :: (MonadIO io, Monad m) => io (Button m)
mkBlock = mkButton . const $ pure ()
