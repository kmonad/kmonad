{-|
Module      : KMonad.Keyboard
Description : Basic keyboard types
Copyright   : (c) David Janssen, 2019
License     : MIT
Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : portable

This module contains or reexports all the basic, non-IO concepts related to
dealing with key codes, events, and mappings. For keyboard-related IO see
"KMonad.Keyboard.IO".

-}
module KMonad.Keyboard
  ( -- * Keyactions and their helpers
    -- $keyaction
    SwitchAction(..)
  , KeyAction
  , HasKeyAction(..)
  , mkKeyAction
  , keyPress
  , keyRelease
  , isPress
  , isRelease
  , kaEq
  , KeyEvent

  , module KMonad.Keyboard.Keycode
  )

where

import KPrelude

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

instance Display KeyAction where
  textDisplay a = tshow (a^.switchAction) <> " " <> textDisplay (a^.keycode)

instance Ord KeyAction where
  a `compare` b = case (a^.switchAction) `compare` (b^.switchAction) of
    EQ -> (a^.keycode) `compare` (b^.keycode)
    x  -> x

mkKeyAction :: SwitchAction -> Keycode -> KeyAction
mkKeyAction = KeyAction

-- | Create a 'KeyAction' that represents pressing a key
keyPress :: Keycode -> KeyAction
keyPress = KeyAction Press

-- | Create a 'KeyAction' that represents releaseing a key
keyRelease :: Keycode -> KeyAction
keyRelease = KeyAction Release

-- | Test if a thing with a 'SwitchAction' is/has a 'Press'
isPress :: HasKeyAction a => a -> Bool
isPress a = a^.switchAction == Press

-- | Test if a thing with a 'SwitchAction' is/has a 'Release'
isRelease :: HasKeyAction a => a -> Bool
isRelease a = a^.switchAction == Release

-- | Run a predicate over the 'KeyAction' part of a structure
kaEq :: (HasKeyAction a, HasKeyAction b) => a -> b -> Bool
kaEq a b = (a^.keyAction) == (b^.keyAction)


--------------------------------------------------------------------------------
-- $ev

type KeyEvent = Timed KeyAction

instance HasKeyAction KeyEvent where keyAction = thing
