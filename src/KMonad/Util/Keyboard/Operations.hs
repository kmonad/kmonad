module KMonad.Util.Keyboard.Operations

where

import KMonad.Prelude
import KMonad.Util.Keyboard.Types
import KMonad.Util.Keyboard.OS (keycodeNames)

import qualified RIO.HashMap as M

--------------------------------------------------------------------------------
-- $lookup

-- | A function used to lookup 'Keycode's by name
--
-- NOTE: Only intented to be used with static arguments in the Haskell code to
-- /get/ at things like the keycode for <Tab> across platforms. Do not expose
-- this to user-input.
--
-- One of the few functions that will just throw an exception.
--
kc :: CoreName -> Keycode
kc n = case M.lookup n keycodeNames of
  Just c  -> c
  Nothing -> throw $ NoSuchKeynameException (unCore n)

--------------------------------------------------------------------------------
-- $shorthand

mkPress :: Keycode -> KeySwitch
mkPress = mkKeySwitch Press

mkRelease :: Keycode -> KeySwitch
mkRelease = mkKeySwitch Release

--------------------------------------------------------------------------------
-- $preds

-- | Return whether the provided KeyEvent is a Press
isPress :: (HasSwitch a) => Pred a
isPress = (== Press) . view switch

-- | Return whether the provided KeyEvent is a Release
isRelease :: (HasSwitch a) => Pred a
isRelease = not . isPress

-- | Return whether the provided KeyEvent matches a particular Keycode
isKeycode :: (HasCode a) => Keycode -> Pred a
isKeycode c = (== c) . view code

isReleaseOf :: (HasKeySwitch a) => Keycode -> Pred a
isReleaseOf c a = a^.keySwitch == mkRelease c
