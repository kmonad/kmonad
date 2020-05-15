{-# LANGUAGE DeriveAnyClass #-}
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
  ( -- * KeyEvents and their helpers
    -- $event
    Switch(..)
  , KeyEvent
  , switch
  , keycode
  , mkKeyEvent
  , mkPress
  , mkRelease

    -- * LMaps
    -- $lmap
  , LayerTag
  , LMap

    -- * Reexports
  , module KMonad.Keyboard.Keycode
  )

where

import KPrelude

import KMonad.Keyboard.Keycode

import qualified Data.LayerStack as Ls


--------------------------------------------------------------------------------
-- $event
--
-- An 'KeyEvent' in 'KMonad' is either the 'Press' or 'Release' of a particular
-- 'Keycode'. A complete list of keycodes can be found in
-- "KMonad.Keyboard.Keycode".

-- | KMonad recognizes 2 different types of actions: presses and releases. Note
-- that we do not handle repeat events at all.
data Switch
  = Press
  | Release
  deriving (Eq, Ord, Show, Enum, Generic, Hashable)

-- | An 'KeyEvent' is a 'Switch' on a particular 'Keycode'
data KeyEvent = KeyEvent
  { _switch  :: Switch
  , _keycode :: Keycode
  } deriving (Eq, Show, Generic, Hashable)
makeLenses ''KeyEvent

-- | A 'Display' instance for 'KeyEvent's that prints them out nicely.
instance Display KeyEvent where
  textDisplay a = tshow (a^.switch) <> " " <> textDisplay (a^.keycode)

-- | An 'Ord' instance, where Press > Release, and otherwise we 'Ord' on the
-- 'Keycode'
instance Ord KeyEvent where
  a `compare` b = case (a^.switch) `compare` (b^.switch) of
    EQ -> (a^.keycode) `compare` (b^.keycode)
    x  -> x

-- | Create a new 'KeyEvent' from a 'Switch' and a 'Keycode'
mkKeyEvent :: Switch -> Keycode -> KeyEvent
mkKeyEvent = KeyEvent

-- | Create a 'KeyEvent' that represents pressing a key
mkPress :: Keycode -> KeyEvent
mkPress = KeyEvent Press

-- | Create a 'KeyEvent' that represents releaseing a key
mkRelease :: Keycode -> KeyEvent
mkRelease = KeyEvent Release


--------------------------------------------------------------------------------
-- $lmap
--
-- Type aliases for specifying stacked-layer mappings

-- | Layers are identified by a tag that is simply a 'Text' value.
type LayerTag = Text

-- | 'LMap's are mappings from 'LayerTag'd maps from 'Keycode' to things.
type LMap a = Ls.LayerStack LayerTag Keycode a
