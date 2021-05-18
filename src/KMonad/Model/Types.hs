module KMonad.Model.Types

where

import KMonad.Prelude
import KMonad.Util.Keyboard
import qualified KMonad.Util.LayerStack as Ls

type LayerTag = Text
type KeyPred = KeySwitch -> Bool

-- | 'LMap's are mappings from 'LayerTag'd maps from 'Keycode' to things.
type LMap a = Ls.LayerStack LayerTag Keycode a
