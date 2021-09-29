module KMonad.Util.Keyboard.Common
  ( -- * $switch
    Switch(..)
  , HasSwitch(..)

    -- * $code
  , Keycode
  , HasCode(..)

    -- * $name
  , Keyname
  , keycodeNames
  , kc

    -- * $keyswitch
  , KeySwitch
  , HasKeySwitch(..)
  , mkKeySwitch

    -- * $keyevent
  , KeyEvent
  , mkKeyEvent
  , keySwitchNow
  )
where

import KMonad.Prelude
import KMonad.Util.Name
import KMonad.Util.Time
import KMonad.Util.Keyboard.OS (Keycode, keycodeNames)

import RIO.Partial (fromJust)
import qualified RIO.HashMap as M

--------------------------------------------------------------------------------
-- $switch

-- | Differentiates between 'Press' and 'Release' events
data Switch
  = Press
  | Release
  deriving (Eq, Show)

-- | A class describing how to get at somethign containing a 'Switch'
class HasSwitch a where switch :: Lens' a Switch
instance HasSwitch Switch where switch = id

--------------------------------------------------------------------------------
-- $code
--
-- NOTE: OS-specific 'Keycode' type imported from separate module

-- | A class describing how to get at something containing a 'Keycode'
class HasCode a where code :: Lens' a Keycode
instance HasCode Keycode where code = id

--------------------------------------------------------------------------------
-- $name

-- | Type alias for names that are applied to keycodes
type Keyname = Name

-- | The error that is thrown when we encounter an unknown Keyname.
data NoSuchKeynameException = NoSuchKeynameException Keyname deriving Show
instance Exception NoSuchKeynameException where
  displayException (NoSuchKeynameException n) =
    "Encountered unknown keyname in code: " <> unpack n

-- | A function used to lookup 'Keycode's by name
--
-- NOTE: Only intented to be used with static arguments in the Haskell code to
-- /get/ at things like the keycode for <Tab> across platforms. Do not expose
-- this to user-input.
--
-- One of the few functions that will just throw an exception.
--
kc :: Keyname -> Keycode
kc n = case M.lookup n keycodeNames of
  Just c  -> c
  Nothing -> throw $ NoSuchKeynameException n


--------------------------------------------------------------------------------
-- $keyswitch

-- | Record indicating a switch-change for a particular keycode
data KeySwitch = KeySwitch
  { _kSwitch :: Switch
  , _kCode   :: Keycode
  } deriving (Eq, Show)
makeLenses ''KeySwitch

class HasKeySwitch a where keySwitch :: Lens' a KeySwitch

instance HasKeySwitch KeySwitch where keySwitch = id
instance HasSwitch    KeySwitch where switch    = kSwitch
instance HasCode      KeySwitch where code      = kCode

-- | Constructor used to make 'KeySwitch' data
mkKeySwitch :: Switch -> Keycode -> KeySwitch
mkKeySwitch = KeySwitch

--------------------------------------------------------------------------------
-- $keyevent

-- | The event of some switch for some keycode at some time.
data KeyEvent = KeyEvent
  { _eKeySwitch :: KeySwitch
  , _eTime      :: Time
  } deriving (Eq, Show)
makeLenses ''KeyEvent

instance HasKeySwitch KeyEvent where keySwitch = eKeySwitch
instance HasSwitch    KeyEvent where switch    = keySwitch.switch
instance HasCode      KeyEvent where code      = keySwitch.code
instance HasTime      KeyEvent where time      = eTime

-- | A constructor for new 'KeyEvent's
mkKeyEvent :: Switch -> Keycode -> Time -> KeyEvent
mkKeyEvent s c = KeyEvent (KeySwitch s c)

-- | Create a 'KeyEvent' matching a 'KeySwitch' at the current time
keySwitchNow :: IO m => KeySwitch -> m KeyEvent
keySwitchNow = now . KeyEvent
