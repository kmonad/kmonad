{-|
Module      : KMonad.Domain.Button.Buttons.After
Description : A button that performs 2 button taps in a row
Copyright   : (c) David Janssen, 2019
License     : MIT

Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : non-portable (MPTC with FD, FFI to Linux-only c-code)

A button responds to a press by tapping 2 other buttons in succession.

-}
module KMonad.Domain.Button.Buttons.After
  ( mkAfter
  )
where

import Control.Monad.IO.Class
import KMonad.Core
import KMonad.Domain.Button.Button
import KMonad.Domain.Effect

-- | Return a button that manages two other buttons, A and B, and turns a Press
-- into TapA >> TapB and does nothing on release
mkAfter :: (MonadIO io, MonadButton m)
  => Button m      -- ^ The button to run first
  -> Button m      -- ^ The button to run second
  -> io (Button m) -- ^ The resulting button
mkAfter a b = mkButton $ \case
  Engaged    -> tap a >> press b
  Disengaged -> release b
