{-|
Module      : KMonad.Core.Keyboard.Event
Description : The basic hierarchy of Keyboard events
Copyright   : (c) David Janssen, 2019
License     : MIT

Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : non-portable (MPTC with FD, FFI to Linux-only c-code)
-}
module KMonad.Core.Keyboard.Event
  ( -- * Switch actions
    -- $switch
    SwitchAction(..)
  , HasSwitchAction(..)

    -- * Key actions
    -- $keyaction
  , KeyAction(..)

    -- * Key events
    -- $keyevent
  , KeyEvent(..)
  , mkKeyEvent
  )
where

import KMonad.Prelude

import KMonad.Core.Keyboard.Keycode
import KMonad.Core.Time

import Data.Serialize

--------------------------------------------------------------------------------
-- $switch

-- | KMonad recognizes 2 different types of actions: presses and releases
data SwitchAction
  = Press
  | Release
  deriving (Eq, Ord, Show, Enum, Generic)
makeClassyPrisms ''SwitchAction

instance Serialize SwitchAction

class HasSwitchAction a where
  switchAction :: Lens' a SwitchAction


--------------------------------------------------------------------------------
-- $keyaction

-- | A 'KeyAction' is a 'SwitchAction' on a particular 'Keycode'
data KeyAction = KeyAction
  { _kaSwitchAction :: SwitchAction
  , _kaKeycode      :: Keycode
  } deriving (Eq, Show, Generic)
makeClassy ''KeyAction

instance Serialize KeyAction
instance HasSwitchAction KeyAction where switchAction = kaSwitchAction
instance HasKeycode      KeyAction where keycode      = kaKeycode

instance Ord KeyAction where
  a `compare` b = case (a^.switchAction) `compare` (b^.switchAction) of
    EQ -> (a^.keycode) `compare` (b^.keycode)
    x  -> x


--------------------------------------------------------------------------------
-- $keyevent

-- | A KeyEvent is a KeyAction that occurred at a particular time
data KeyEvent = KeyEvent
  { _evKeyAction :: KeyAction
  , _evTime      :: Time
  } deriving (Eq, Show, Generic)
makeClassy ''KeyEvent


instance Serialize KeyEvent

-- | Hook up all the classy lenses
instance HasKeyAction    KeyEvent where keyAction    = evKeyAction
-- instance HasSwitchAction KeyEvent where switchAction = keyAction.switchAction
-- instance HasKeycode      KeyEvent where keycode      = keyAction.keycode
instance HasTime         KeyEvent where time         = evTime

-- | Create a KeyEvent
mkKeyEvent :: SwitchAction -> Keycode -> Time -> KeyEvent
mkKeyEvent a c = KeyEvent (KeyAction a c)


