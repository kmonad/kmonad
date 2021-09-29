module KMonad.Util.Keyboard.Windows
  ( Keycode (..),

    -- * Event types
    EvType (..),
    _EvType,

    -- * Raw events as defined in Linux
    RawEvent (..),
    HasRawEvent (..),
    mkRaw,

    -- * Names for keycodes
    keycodeNames,
  )
where

import Foreign.Storable
import KMonad.Prelude
import KMonad.Util.Keyboard.Common
import KMonad.Util.Keyboard.Keynames
import qualified RIO.HashMap as M

--------------------------------------------------------------------------------

-- $keycode

-- | In Windows we use 'Word32', the windows-native keycode type
newtype Keycode = Keycode {unKeycode :: Word32}
  deriving (Eq, Ord, Num, Show, Enum, Hashable)

--------------------------------------------------------------------------------

-- $evtyp

-- | Windows uses 2 types of events, presses and releases.
--
-- In windows, repeat events are signalled by repeated press events. The type is
-- ordered in a way to line up with the integers Windows uses to encode for
-- there events. I.e. 0=Press, 1=Release
data EvType = WindowsPress | WindowsRelease
  deriving (Eq, Show, Enum)

-- | Prism between Linux and Haskell representation of event-types
_EvType :: Prism' Word8 EvType
_EvType = prism' (fi . fromEnum) $ \i ->
  [WindowsPress, WindowsRelease] ^? ix (fi i)

--------------------------------------------------------------------------------

-- $raw

-- | The RawEvent datatype
--
-- It contains a 'Word8' signifying whether the event was a Press (0) or Release
-- (1), and a 'Word32' (Windows @DWORD@) signifying the keycode value.
data RawEvent = RawEvent
  { -- | press or release
    _reVal :: Word8,
    -- | keycode
    _reCode :: Word32
  }
  deriving (Eq, Ord, Show)

makeClassy ''RawEvent

-- | This lets us send 'WinKeyEvent's between Haskell and C.
instance Storable RawEvent where
  alignment _ = 4 -- lowest common denominator of: 1 4
  sizeOf _ = 8 -- (1 + 3-padding) + 4
  peek ptr = do
    s <- peekByteOff ptr 0
    c <- peekByteOff ptr 4
    return $ RawEvent s c
  poke ptr (RawEvent s c) = do
    pokeByteOff ptr 0 s
    pokeByteOff ptr 4 c

-- | Make a new raw event
mkRaw :: EvType -> Keycode -> RawEvent
mkRaw p c =
  RawEvent
    { _reCode = unKeycode c,
      _reVal = p ^. re _EvType
    }

--------------------------------------------------------------------------------

-- $keyname

keycodeNames :: M.HashMap CoreName Keycode
keycodeNames =
  mconcat
    [ wnNumbers,
      wnLetters,
      wnPunct,
      wnFKeys,
      wnKPNums,
      wnKPSymb,
      wnOther,
      wnMods
    ]

-- | zip 2 lists if they have the same length, only ever use this to generate
-- static data.
z :: [CoreName] -> [Keycode] -> M.HashMap CoreName Keycode
z a b
  | length a == length b = M.fromList $ zip a b
  | otherwise = error $ "length error" <> show a <> show b

type WinNames = M.HashMap CoreName Keycode

wnNumbers, wnLetters, wnPunct, wnMods, wnFKeys, wnKPNums, wnKPSymb, wnOther :: WinNames
wnNumbers = z knNumbers [0x30 .. 0x39]
wnLetters = z knLetters [0x41 .. 0x5A]
wnPunct =
  M.fromList
    [ (";", 0xBA),
      ("=", 0xBB),
      (",", 0xBC),
      ("-", 0xBD),
      (".", 0xBE),
      ("/", 0xBF),
      ("`", 0xC0),
      ("[", 0xDB),
      ("\\", 0xDC),
      ("]", 0xDD),
      ("'", 0xDE)
    ]
wnMods =
  M.fromList
    [ ("lsft", 0xA0),
      ("rsft", 0xA1),
      ("lctl", 0xA2),
      ("rctl", 0xA3),
      ("lalt", 0xA4),
      ("ralt", 0xA5),
      ("lmet", 0x5B),
      ("rmet", 0x5C)
    ]
wnFKeys = z knFKeys [0x70 .. 0x7B]
wnKPNums = z knKPNums [0x60 .. 0x69]
wnKPSymb =
  M.fromList
    [ ("kp*", 0x6A),
      ("kp+", 0x6B),
      ("kp-", 0x6D),
      ("kp.", 0x6E),
      ("kp/", 0x6F),
      ("kprt", 0x0D) -- Sadly can't distinguish from normal ret on Windows
    ]
wnOther =
  M.fromList
    [ ("esc", 0x1B),
      ("tab", 0x09),
      ("ret", 0x0D),
      ("bspc", 0x08),
      ("caps", 0x14),
      ("nlck", 0x90),
      ("slck", 0x91),
      ("spc", 0x20),
      ("102d", 0xE2),
      ("sys", 0x2C),
      ("pgdn", 0x22),
      ("pgup", 0x21),
      ("home", 0x24),
      ("end", 0x23),
      ("up", 0x26),
      ("down", 0x28),
      ("left", 0x25),
      ("rght", 0x27),
      ("ins", 0x2D),
      ("del", 0x2E),
      ("paus", 0x13),
      ("cmps", 0x5D)
    ]
