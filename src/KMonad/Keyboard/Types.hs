{-# LANGUAGE DeriveAnyClass #-}
module KMonad.Keyboard.Types
  (
    Switch(..)
  , KeyEvent
  , mkKeyEvent
  , HasKeyEvent(..)
  , KeyPred
  , LayerTag
  , LMap
  )
where

import KMonad.Prelude
import KMonad.Keyboard.Keycode

import qualified KMonad.Util.LayerStack as Ls

--------------------------------------------------------------------------------
-- $event
--
-- An 'KeyEvent' in KMonad is either the 'Press' or 'Release' of a particular
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
  { _switch  :: Switch  -- ^ Whether the 'KeyEvent' was a 'Press' or 'Release'
  , _keycode :: Keycode -- ^ The 'Keycode' mapped to this 'KeyEvent'
  } deriving (Eq, Show, Generic, Hashable)
makeClassy ''KeyEvent

-- | Create a new 'KeyEvent' from a 'Switch' and a 'Keycode'
mkKeyEvent :: Switch -> Keycode -> KeyEvent
mkKeyEvent = KeyEvent

-- | A 'Display' instance for 'KeyEvent's that prints them out nicely.
instance Display KeyEvent where
  textDisplay a = tshow (a^.switch) <> " " <> textDisplay (a^.keycode)

-- | An 'Ord' instance, where Press > Release, and otherwise we 'Ord' on the
-- 'Keycode'
instance Ord KeyEvent where
  a `compare` b = case (a^.switch) `compare` (b^.switch) of
    EQ -> (a^.keycode) `compare` (b^.keycode)
    x  -> x


-- | Predicate on KeyEvent's
type KeyPred = KeyEvent -> Bool

--------------------------------------------------------------------------------
-- $lmap
--
-- Type aliases for specifying stacked-layer mappings

-- | Layers are identified by a tag that is simply a 'Text' value.
type LayerTag = Text

-- | 'LMap's are mappings from 'LayerTag'd maps from 'Keycode' to things.
type LMap a = Ls.LayerStack LayerTag Keycode a
