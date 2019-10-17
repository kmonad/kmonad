{-|
Module      : KMonad.Core.Button
Description : The pure types and utilities for Button data.
Copyright   : (c) David Janssen, 2019
License     : MIT

Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : non-portable (MPTC with FD, FFI to Linux-only c-code)

FIXME: Update documentation

-}
module KMonad.Core.Button
  ( -- * Core types and constructors
    -- $types
    Button
  , mkButton
  , runButton

    -- * Utilities for working with Buttons
    -- $util
  , press, release, tap
  )
where


import Control.Lens
import Control.Monad.State
import KMonad.Core.Keyboard
import KMonad.Core.Switch
import KMonad.Core.Types

--------------------------------------------------------------------------------
-- $types

-- | Buttons are actions in a particular context that are triggered by
-- switch-states.
--
-- @since 0.1.0
newtype Button m = Button { unButton :: SwitchState -> m ()}

-- | Create a new 'Button' from an effectful function from 'SwitchState'
--
-- @since 0.1.0
mkButton :: (SwitchState -> m ()) -> Button m
mkButton = Button

-- | Create a new 'Button' registered to a particular KeyCode from an effectful
-- function 'SwitchState'

-- | Run a 'Button' with the provided 'SwitchState'
--
-- @since 0.1.0
runButton :: Button m -> SwitchState -> m ()
runButton = unButton


--------------------------------------------------------------------------------
-- $util


-- | Trigger a 'Button' by passing a 'Engaged'
--
-- @since 0.1.0
press :: Button m -> m ()
press = ($ Engaged) . runButton

-- | Trigger a 'Button' by passing a 'Disengaged'
--
-- @since 0.1.0
release :: Button m -> m ()
release = ($ Disengaged) . runButton

-- | Trigger a 'Button' twice, first with a 'Engaged', then with a 'Disengaged'
--
-- @since 0.1.0
tap :: Applicative m => Button m -> m ()
tap b = press b *> release b


--------------------------------------------------------------------------------
