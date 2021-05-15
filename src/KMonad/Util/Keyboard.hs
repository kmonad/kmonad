{- Code related to working with keyboards -}
module KMonad.Util.Keyboard

where

import KMonad.Prelude
import KMonad.Util.Time

-- | Differentiates between 'Press' and 'Release' events
data Switch
  = Press
  | Release
  deriving (Eq, Show)
makeClassyPrisms ''Switch

class HasSwitch a where switch :: Lens' a Switch
instance HasSwitch Switch where switch = id


-- | An Event, still parametrized by the type used to indicate the keycode.
data Event' c = Event
  { _eSwitch :: Switch
  , _eCode   :: c
  , _eTime   :: Time
  } deriving (Eq, Show)
makeLenses ''Event'

instance HasSwitch (Event' c) where switch = eSwitch
instance HasTime   (Event' c) where time   = eTime
-- NOTE: HasCode is defined in OS-specific implementations
