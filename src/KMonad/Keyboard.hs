{-# LANGUAGE DeriveAnyClass #-}
module KMonad.Keyboard
  ( -- * Keyactions and their helpers
    -- $keyaction
    SwitchAction(..)
  , KeyAction
  , switchAction
  , keycode
  , pressKey
  , releaseKey
  , isPress
  , isRelease
  , KeyEvent

  , module KMonad.Keyboard.Keycode
  )

where

import Prelude

import KMonad.Keyboard.Keycode
import KMonad.Util


--------------------------------------------------------------------------------
-- $keyaction
--
-- 'KeyAction's are either a 'Press' or 'Release' of a particular 'Keycode'.
-- They are used internally to represent both the events coming into kmonad, as
-- well as the events being emitted from kmonad.

-- | KMonad recognizes 2 different types of actions: presses and releases
data SwitchAction
  = Press
  | Release
  deriving (Eq, Ord, Show, Enum, Generic, Hashable)

instance Serialize SwitchAction

-- | A 'KeyAction' is a 'SwitchAction' on a particular 'Keycode'
data KeyAction = KeyAction
  { _switchAction :: SwitchAction
  , _keycode      :: Keycode
  } deriving (Eq, Show, Generic, Hashable)
makeClassy ''KeyAction

instance Serialize KeyAction

instance PrettyPrint KeyAction where
  pprint a = tshow (a^.switchAction) <> " " <> pprint (a^.keycode)

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

-- | Test if a thing with a 'SwitchAction' is/has a 'Press'
isPress :: HasKeyAction a => a -> Bool
isPress a = a^.switchAction == Press

-- | Test if a thing with a 'SwitchAction' is/has a 'Release'
isRelease :: HasKeyAction a => a -> Bool
isRelease a = a^.switchAction == Release

--------------------------------------------------------------------------------
-- $ev

type KeyEvent = Timed KeyAction

instance HasKeyAction KeyEvent where keyAction = thing
