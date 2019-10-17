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
  , mkBlockM
  )
where

import KMonad.Core.Button


-- | Create a Button that does nothing
mkBlock :: Monad m => Button m
mkBlock = mkButton . const $ return ()

-- | Return a Button that does nothing from an arbitrary Monad
mkBlockM :: (Monad m, Monad n) => n (Button m)
mkBlockM = return mkBlock
