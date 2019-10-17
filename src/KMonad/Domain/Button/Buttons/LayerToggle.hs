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
  ( LTCtx
  , mkLayerToggleM
  )
where

import Control.Lens
import Control.Monad.Reader

import KMonad.Core
import KMonad.Domain.Effect

-- | The context required by LayerToggle buttons
type LTCtx m =
  ( MonadTrace      m
  , MonadVar        m
  , MonadStackManip m
  )

data BState
  = Unpressed
  | Pressed
  deriving (Eq, Show)

-- | Return a 'Button' that pushes a layer onto the stack when pressed, and pops
-- it when released.
mkLayerToggleM :: (LTCtx m, MonadIO n) => Name -> n (Button m)
mkLayerToggleM lid = runVar Unpressed $ \st ->
                       mkButton $ \x -> go st lid x

go :: LTCtx m => Var BState -> Name -> SwitchState -> m ()
go st lid x = do
  v <- getVar st
  case (v, x) of
    -- Pressing an unpressed button
    (Unpressed, Engaged) -> do
      putVar Pressed st
      trace $ "pushing layer: " <> lid
      pushL lid
    -- Releasing a pressed button
    (Pressed, Disengaged) -> do
      putVar Unpressed st
      trace $ "popping layer: " <> lid
      popL lid
    -- Anything else
    _                   -> putVar v st
