module KMonad.Types.Keymap

where

import KMonad.Prelude

import KMonad.Types.Button
import KMonad.Types.Keyboard.Event
import KMonad.Types.MapStack
import KMonad.Types.Name

import qualified RIO.HashMap as M

--------------------------------------------------------------------------------
-- $maps
--

-- | A mapping from 'Keycode's to 'Button's
data LayerCfg = LayerCfg
  { _lcName    :: !Name
  , _lcButtons :: !(M.HashMap Keycode Button)
  }

-- |
type Layer = M.HashMap Keycode Button


-- | A 'KeyMap' is a collection of 'Name'd maps from 'Keycode's to 'Button's
type KeyMap = MapStack Name Keycode Button
