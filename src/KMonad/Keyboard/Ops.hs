module KMonad.Keyboard.Ops
  ( switch
  , keycode
  , mkKeyEvent
  , mkPress
  , mkRelease

    -- * Predicates
  , isPress
  , isRelease
  , isKeycode
  , isPressOf
  , isReleaseOf
  )
where

import KMonad.Prelude
import KMonad.Keyboard.Types
import KMonad.Keyboard.Keycode



-- | Create a 'KeyEvent' that represents pressing a key
mkPress :: Keycode -> KeyEvent
mkPress = mkKeyEvent Press

-- | Create a 'KeyEvent' that represents releaseing a key
mkRelease :: Keycode -> KeyEvent
mkRelease = mkKeyEvent Release

-- | Return whether the provided KeyEvent is a Press
isPress :: KeyPred
isPress = (== Press) . view switch

-- | Return whether the provided KeyEvent is a Release
isRelease :: KeyPred
isRelease = not . isPress

-- | Return whether the provided KeyEvent matches a particular Keycode
isKeycode :: Keycode -> KeyPred
isKeycode c = (== c) . view keycode

-- | Returth whether the provided KeyEvent matches the release of the Keycode
isReleaseOf :: Keycode -> KeyPred
isReleaseOf = (==) . mkRelease

-- | Return whether the provided KeyEvent matches the press of the Keycode
isPressOf :: Keycode -> KeyPred
isPressOf = (==) . mkPress
