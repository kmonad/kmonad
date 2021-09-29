module KMonad.Util.Keyboard.Common
  ( CoreName(..) )
where

{- NOTE:

Regard this module as a part of KMonad.Util.Keyboard.Types *except* that it can
be imported by the OS-specific modules (whereas Types gets imported by them, so
the can't import it.) Types reexports this module, so there is no need to ever
import it manually except for in OS-specific modules.

This module is unable to import anything from within the 'KMonad.Util.Keyboard'
module.

-}

import KMonad.Prelude

--------------------------------------------------------------------------------
-- $core

-- | The CoreName type, used to denote the primary name for a keycode.
--
-- We provide a lot of support for locales and alternative names for keycodes
-- for use in the config language and to support different internationalization
-- standards. But since we use different 'Keycode' types for different OSes, we
-- can't hard-code the names of these 'Keycode's. Therefore we provide a
-- 'CoreName' type, and insist (through `hspec`) that a particular collection of
-- 'CoreName's is bound to unique 'Keycode's.
--
-- Every 'CoreName' in 'knAll' must have a 'Keycode', and every 'Keycode' may
-- have only 1 'CoreName'. If you ever refer to a 'Keycode' in the source-code
-- using 'kc', it must be through its 'CoreName'.
newtype CoreName = CoreName { unCore :: Text }
  deriving (Eq, Ord, IsString, Hashable)

instance Show    CoreName where show        = show        . unCore
instance Display CoreName where textDisplay = textDisplay . unCore
