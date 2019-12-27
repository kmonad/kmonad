{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-|
Module      : KMonad.Types.Keyboard.Event
Description : The basic hierarchy of Keyboard events
Copyright   : (c) David Janssen, 2019
License     : MIT

Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : non-portable (MPTC with FD, FFI to Linux-only c-code)
-}
module KMonad.Types.Keyboard.Event2
  -- ( -- * Switch actions
  --   -- $switch
  --   SwitchAction(..)
  -- , HasSwitchAction(..)
  -- , isPress
  -- , isRelease


  -- --   -- * Key actions
  -- --   -- $keyaction
  -- -- , KeyAction(..)
  -- -- , HasKeyAction(..)
  -- -- , pressKey
  -- -- , releaseKey

  -- --   -- * Key events
  -- --   -- $keyevent
  -- -- , KeyEvent(..)
  -- -- , HasKeyEvent(..)
  -- -- , mkKeyEvent

  --   -- * Also export "KMonad.Types.Keycode"
  -- , module KMonad.Types.Keyboard.Keycode
  -- )
where

import KMonad.Prelude

import KMonad.Types.Keyboard.Keycode
import KMonad.Types.Time

import Data.Serialize

--------------------------------------------------------------------------------
-- $switch

-- | KMonad recognizes 2 different types of actions: presses and releases
data SwitchAction
  = Press
  | Release
  deriving (Eq, Ord, Show, Enum, Generic, Hashable)
makeClassyPrisms ''SwitchAction

instance Serialize SwitchAction

class HasSwitchAction e where
  switchAction :: Lens' e SwitchAction

instance HasSwitchAction SwitchAction where
  switchAction = id

-- | Test if a thing with a 'SwitchAction' is/has a 'Press'
isPress :: HasSwitchAction a => a -> Bool
isPress a = a^.switchAction == Press

-- | Test if a thing with a 'SwitchAction' is/has a 'Release'
isRelease :: HasSwitchAction a => a -> Bool
isRelease a = a^.switchAction == Release


--------------------------------------------------------------------------------
-- $keyaction

-- | A 'KeyAction' is a 'SwitchAction' on a particular 'Keycode'
data KeyAction = KeyAction
  { _kaSwitchAction :: SwitchAction
  , _kaKeycode      :: Keycode
  } deriving (Eq, Show, Generic, Hashable)
makeLenses ''KeyAction

class (HasKeycode e, HasSwitchAction e) => HasKeyAction e where
  keyAction :: Lens' e KeyAction
instance HasKeyAction KeyAction where keyAction    = id

-- instance HasSwitchAction e

instance HasKeyAction e => HasSwitchAction e where
  switchAction = keyAction . switchAction
instance HasKeyAction e => HasKeycode e where
  keycode = keyAction . keycode



instance Serialize KeyAction

instance Ord KeyAction where
  a `compare` b = case (a^.switchAction) `compare` (b^.switchAction) of
    EQ -> (a^.keycode) `compare` (b^.keycode)
    x  -> x

-- | Create a 'KeyAction' that represents pressing a key
pressKey :: Keycode -> KeyAction
pressKey = KeyAction Press


-- | Create a 'KeyAction' that represents releaseing a key
releaseKey :: Keycode -> KeyAction
releaseKey = KeyAction Release

--------------------------------------------------------------------------------
-- $keyevent

-- | A KeyEvent is a KeyAction that occurred at a particular time
data KeyEvent = KeyEvent
  { _evKeyAction :: KeyAction
  , _evTime      :: Time
  } deriving (Eq, Show, Generic)
makeLenses ''KeyEvent

class (HasKeyAction e, HasTime e) => HasKeyEvent e where
  keyEvent :: Lens' e KeyEvent

instance HasKeyEvent KeyEvent where keyEvent = id
-- instance HasKeyAction KeyEvent where keyAction = evKeyAction
-- instance HasTime KeyEvent where time = evTime

instance HasKeyEvent e => HasKeyAction e where
  keyAction = keyEvent . evKeyAction
instance HasKeyEvent e => HasTime e where
  time = keyEvent . time

-- instance HasKeycode      KeyEvent where keycode      = keyAction.keycode
-- instance HasSwitchAction KeyEvent where switchAction = keyAction.switchAction
-- instance HasKeyAction    KeyEvent where keyAction    = evKeyAction
-- instance HasTime         KeyEvent where time         = evTime
-- instance HasKeyEvent     KeyEvent where keyEvent     = id

-- instance Serialize KeyEvent

-- -- | Create a KeyEvent
-- mkKeyEvent :: SwitchAction -> Keycode -> Time -> KeyEvent
-- mkKeyEvent a c = KeyEvent (KeyAction a c)
