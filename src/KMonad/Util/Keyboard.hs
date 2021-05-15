{- Code related to working with keyboards -}
module KMonad.Util.Keyboard
  ( Switch(..)
  , HasSwitch(..)

  , Keycode
  , HasCode(..)
  , keycodeNames
  , kc

  , KeyEvent
  , mkKeyEvent
  )
where

import KMonad.Prelude
import KMonad.Util.Name
import KMonad.Util.Time
import KMonad.Util.Keyboard.OS (Keycode, keycodeNames)

import RIO.Partial (fromJust)
import qualified RIO.HashMap as M

-- | Differentiates between 'Press' and 'Release' events
data Switch
  = Press
  | Release
  deriving (Eq, Show)

-- | A class describing how to get at somethign containing a 'Switch'
class HasSwitch a where switch :: Lens' a Switch
instance HasSwitch Switch where switch = id

-- | A class describing how to get at something containing a 'Keycode'
class HasCode a where code :: Lens' a Keycode
instance HasCode Keycode where code = id

-- | A function used to lookup 'Keycode's by name
--
-- NOTE: Only intented to be used with static arguments in the Haskell code to
-- /get/ at things like the keycode for <Tab> across platforms.
--
-- One of the few functions that will just throw an exception.
--
kc :: Name -> Keycode
kc = fromJust . flip M.lookup keycodeNames

-- | The event of some switch for some keycode at some time.
data KeyEvent = KeyEvent
  { _eSwitch :: Switch
  , _eCode   :: Keycode
  , _eTime   :: Time
  } deriving (Eq, Show)
makeLenses ''KeyEvent

-- | A constructor for new 'KeyEvent's
mkKeyEvent :: Switch -> Keycode -> Time -> KeyEvent
mkKeyEvent = KeyEvent

instance HasSwitch KeyEvent where switch = eSwitch
instance HasTime   KeyEvent where time   = eTime
instance HasCode   KeyEvent where code   = eCode
