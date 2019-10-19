{-|
Module      : KMonad.Domain.Button.Buttons.Around
Description : A button that nests two other buttons
Copyright   : (c) David Janssen, 2019
License     : MIT

Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : non-portable (MPTC with FD, FFI to Linux-only c-code)

A button that nests 1 button inside another.

-}
module KMonad.Domain.Button.Buttons.Around
  ( mkAround
  )
where

import Control.Monad.IO.Class
import KMonad.Core
import KMonad.Domain.Button.Button
import KMonad.Domain.Effect

-- | Return a button that manages two other buttons, A and B, and turns a Press
-- into PressA >> PressB and a Release into ReleaseB >> ReleaseA.
mkAround :: (MonadIO io, MonadButton m)
  => Button m      -- ^ The button on the /outside/
  -> Button m      -- ^ The button on the /inside/
  -> io (Button m) -- ^ The resulting button
mkAround a b = mkButton $ \case
  Engaged    -> press a   >> press b
  Disengaged -> release b >> release a
