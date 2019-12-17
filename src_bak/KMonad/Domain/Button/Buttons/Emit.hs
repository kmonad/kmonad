{-|
Module      : KMonad.Domain.Button.Buttons.Emit
Description : A button that emits a 'KeyEvent' on 'Engaged' and 'Disengaged'
Copyright   : (c) David Janssen, 2019
License     : MIT

Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : non-portable (MPTC with FD, FFI to Linux-only c-code)

The 'mkEmit' 'Button' is the standard "this is a button that types a letter"
style button. When pressed, it emits a press for its 'KeyCode', when released it
emits a release for its 'KeyCode'.

-}
module KMonad.Domain.Button.Buttons.Emit
  ( mkEmit
  )
where

import Control.Monad.IO.Class
import KMonad.Core
import KMonad.Domain.Effect
import KMonad.Domain.Button.Button

-- | Return an Emit button
mkEmit :: (MonadIO io, MonadEmit m)
  => KeyCode       -- ^ The keycode to emit on manipulation
  -> io (Button m) -- ^ The resulting button
mkEmit kc = mkButton $ \case
  Engaged    -> emitPress kc
  Disengaged -> emitRelease kc
