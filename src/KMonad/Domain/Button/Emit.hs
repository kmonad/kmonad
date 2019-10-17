{-|
Module      : KMonad.Domain.Button.Emit
Description : A button that emits a 'KeyEvent' on 'Engaged' and 'Disengaged'
Copyright   : (c) David Janssen, 2019
License     : MIT

Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : non-portable (MPTC with FD, FFI to Linux-only c-code)

The 'mkEmit' 'Button' is the standard "this is a button that types a letter"
style button. When pressed, it emits a 'Press' 'KeyEvent' for its 'KeyCode', and
when released, a 'Release' type event.

-}
module KMonad.Domain.Button.Emit
  ( mkEmit
  , mkEmitM
  )
where

import KMonad.Core
import KMonad.Domain.Effect


-- | Return an Emit button
mkEmit :: (MonadEmit m)
  => KeyCode  -- ^ The keycode to emit on manipulation
  -> Button m -- ^ The resulting button
mkEmit kc = mkButton $ \case
  Engaged   -> emitPress kc
  Disengaged -> emitRelease kc

-- | Return an Emit button from within some Monad
mkEmitM :: (MonadEmit m, Monad n)
  => KeyCode      -- ^ The keycode to emit on manipulation
  -> n (Button m) -- ^ The resulting button
mkEmitM = return . mkEmit
