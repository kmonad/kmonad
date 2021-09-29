module KMonad.Util.Keyboard.Linux
  ( -- * Keycode
    Keycode(..)

    -- * Event types
  , EvType(..)
  , _EvType

    -- * Raw events as defined in Linux
  , RawEvent(..)
  , HasRawEvent(..)
  , mkRaw
  , mkSync

    -- * Names for keycodes and linux-specific aliases
  , keycodeNames
  )

where


import KMonad.Prelude

import KMonad.Util.Time
import KMonad.Util.Name
import KMonad.Util.Keyboard.Common

import qualified RIO.HashMap as M

--------------------------------------------------------------------------------
-- $keycode

-- | In Linux we use 'Word16', the linux-native keycode type
newtype Keycode = Keycode { unKeycode :: Word16 }
  deriving (Eq, Ord, Num, Show, Enum, Hashable)

--------------------------------------------------------------------------------
-- $evtype

-- | Linux uses 3 different types of events: Release, Press, Repeat
--
-- The type is ordered in a way to line up with the integers Linux uses to
-- encode for these events. I.e. 0=Release, 1=Press, 2=Repeat.
data EvType = LinuxRelease | LinuxPress | LinuxRepeat
  deriving (Eq, Show, Enum)

-- | Prism between Linux and Haskell representation of event-types
_EvType :: Prism' Int32 EvType
_EvType = prism' (fi . fromEnum) $ \i ->
  ([LinuxRelease, LinuxPress, LinuxRepeat] ^? ix (fi i))

--------------------------------------------------------------------------------
-- $raw

-- | The RawEvent datatype
--
-- Linux produces a stream of binary data representing all its input events
-- through the \/dev\/input files. Each event is represented by 5 numbers:
-- seconds, microseconds, event-type, event-code, and event-value. For more
-- explanation look at: https://www.kernel.org/doc/Documentation/input/input.txt
--
-- We parse the entire event, but discard everything except the keycode.
data RawEvent = RawEvent
  { _leS    :: !Word64  -- ^ The seconds component of system time
  , _leNS   :: !Word64  -- ^ The nanoseconds component of system time
  , _leType :: !Word16  -- ^ The type signals the kind of event (we only use EV_KEY)
  , _leCode :: !Word16  -- ^ The keycode indentifier of the key
  , _leVal  :: !Int32   -- ^ Whether a press, release, or repeat event
  } deriving (Show)
makeClassy ''RawEvent

-- | Generate a new raw event
mkRaw :: IO m => EvType -> Keycode -> m RawEvent
mkRaw p c = do
  t <- getCurrentSystemTime
  pure $ RawEvent
    { _leS    = fi $ t^._s
    , _leNS   = fi $ t^._ns
    , _leType = 1
    , _leCode = unKeycode c
    , _leVal  = p ^. re _EvType
    }

-- | Generate a new sync event
mkSync :: IO m => m RawEvent
mkSync = do
  t <- getCurrentSystemTime
  pure $ RawEvent
    { _leS    = fi $ t^._s
    , _leNS   = fi $ t^._ns
    , _leType = 0
    , _leCode = 0
    , _leVal  = 0
    }

--------------------------------------------------------------------------------
-- $names

z :: [a] -> [b] -> [(b, a)]
z = flip zip

-- | Map of standard names to linux keycodes
keycodeNames :: M.HashMap CoreName Keycode
keycodeNames = M.fromList $
  z [1..83]
    [ "esc", "1", "2", "3", "4", "5", "6", "7", "8", "9", "0" , "-" , "="
    , "bspc", "tab", "q", "w", "e", "r", "t", "y", "u" , "i", "o" , "p", "["
    , "]", "ret", "lctl", "a", "s", "d", "f" , "g", "h", "j" , "k", "l", ";"
    , "'", "`", "lsft", "\\", "z" , "x", "c", "v", "b" , "n", "m", ",", ".", "/"
    , "rsft", "kp*" , "lalt", "spc", "caps" , "f1", "f2", "f3", "f4", "f5", "f6"
    , "f7", "f8", "f9", "f10" , "nlck", "slck", "kp7", "kp8", "kp9" , "kp-"
    , "kp4", "kp5", "kp6" , "kp+", "kp1", "kp2", "kp3", "kp0" , "kp."] <>
  z [85..120]
    [ "zenk", "102d", "f11", "f12", "ro", "kata", "hira", "henk", "kahi"
    , "muhe", "kpj,", "kprt", "rctl", "kp/", "sys", "ralt", "feed", "home"
    , "up", "pgup", "left", "rght", "end", "down", "pgdn", "ins", "del", "macr"
    , "mute", "vold", "volu", "pwr", "kp=", "kp+-", "paus", "scl"
    ] <>
  z [121..182]
    [ "kp,", "hang", "hanj", "yen", "lmet", "rmet", "cmps", "stop", "agn"
    , "prps", "undo", "frnt", "copy", "open", "past", "find", "cut", "help"
    , "menu", "calc", "setp", "slp", "wake", "file", "send", "delf", "xfer"
    , "prg1", "prg2", "www", "msds", "coff", "rot", "cycl" , "mail", "book"
    , "comp", "back", "fwd", "clcd", "ejct", "eccd", "nxsg", "plps", "prvs"
    , "stcd", "rec", "rew", "phon", "iso", "cfg", "hmpg", "refr" , "exit"
    , "move", "edit", "scup", "scdn", "kp(", "kp)", "new", "redo" ] <>
  z [183..194]
    [ "f13", "f14", "f15", "f16", "f17", "f18", "f19", "f20", "f21", "f22"
    , "f23", "f24"] <>
  z [200..248]
    [ "plcd", "pscd", "prg3", "prg4", "dash", "susp", "cls", "play", "ffwd"
    , "bass", "prnt", "hp", "cmra", "soun", "ques", "emal", "chat", "srch"
    , "conn", "fina", "sprt", "shop", "alte", "cncl", "brdn", "brup", "medi"
    , "svid", "kbtg", "kbdn", "kbup", "ksnd", "repl", "fwml", "save", "docs"
    , "batt", "blue", "wlan", "uwb", "unkn", "vdnx", "vdpv", "brcc", "brau"
    , "doff", "wwan", "wmax", "rfkl", "mcmu" ]

