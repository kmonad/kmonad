module KMonad.Core.Switch

where

import Control.Lens

--------------------------------------------------------------------------------

data SwitchState
  = Engaged
  | Disengaged
  deriving (Eq, Ord, Show, Enum)
makeClassyPrisms ''SwitchState

class HasSwitchState a where
  switchState :: Lens' a SwitchState
