module KMonad.Util.Keyboard.Mac
  ( -- * Keycode
    Keycode(..)

    -- * Event types
  , EvType(..)
  , _EvType

    -- * Raw event as defined in Mac
  , RawEvent(..)
  , HasRawEvent(..)
  , mkRaw

    -- * Names for keycodes
  , keycodeNames
  )

where


import KMonad.Prelude

import Numeric (showHex)
import Data.Char (toUpper)

import Foreign.Storable
import KMonad.Util.Keyboard.Common
import KMonad.Util.Keyboard.Keynames

import qualified RIO.HashMap as M
import qualified RIO.Text    as T
import RIO.List.Partial ((!!))


--------------------------------------------------------------------------------
-- $keycode

-- first Word32 represents the IOKit usage page
-- second Word32 represent the IOKit usage
newtype Keycode = Keycode { unKeycode :: (Word32, Word32) }
  deriving (Eq, Ord, Hashable)

instance Show Keycode where
  show (Keycode (p, u)) = "(" ++ toHex p ++ ", " ++ toHex u ++ ")"

toHex :: Word32 -> String
toHex i = "0x" ++ (map toUpper $ showHex i "")

--------------------------------------------------------------------------------
-- $evtype

-- | Mac has two types types of events: Press and Release.
--
-- The type is ordered in a way to line up with the integers our Mac API uses to
-- encode for these events. I.e. 0=Press, 1=Release
data EvType = MacPress | MacRelease
  deriving (Eq, Show, Enum)

-- | Prism between Mac and Haskell representation of event types
_EvType :: Prism' Word64 EvType
_EvType = prism' (fi . fromEnum) $ \i ->
  ([MacPress, MacRelease] ^? ix (fi i))


--------------------------------------------------------------------------------
-- $raw

-- | The RawEvent datatype
--
-- It contains a 'Word64' signifying whether the event was a
-- Press (0) or Release (1), and two 'Word32's (uint32_t)
-- signifying the Mac keycode:
--   first: represents the IOKit usage page.
--   second: represent the IOKit usage.
data RawEvent = RawEvent
  { _reVal  :: Word64  -- ^ press or release
  , _reCode :: (Word32, Word32) -- ^ keycode
  } deriving (Eq, Ord, Show)
makeClassy ''RawEvent

-- | This lets us send key events between Haskell and C.
instance Storable RawEvent where
  alignment _ = 4
  sizeOf    _ = 16
  peek ptr = do
    s <- peekByteOff ptr 0
    p <- peekByteOff ptr 8
    u <- peekByteOff ptr 12
    return $ RawEvent s (p, u)
  poke ptr (RawEvent s (p, u)) = do
    pokeByteOff ptr 0 s
    pokeByteOff ptr 8 p
    pokeByteOff ptr 12 u

-- | Make a new raw event
mkRaw :: EvType -> Keycode -> RawEvent
mkRaw p c = RawEvent
  { _reCode = unKeycode c
  , _reVal  = p ^. re _EvType }


--------------------------------------------------------------------------------
-- $keynames

-- | Takes a usage page, a list of usages and a list of names,
--   and returns a list of tuples of keycodes and their names.
--
-- Caution: Only use this for static data as this throws an error
-- if the lengths of lists differ.
z :: Word32 -> [Word32] -> [Text] -> [(CoreName, Keycode)]
z page codes names
  | length codes == length names = zipWith func codes names
  | otherwise                    = error "mac: keycodes length"
  where
    func c n = (CoreName n, Keycode (page, c))

-- All keycodes are from
--  * https://opensource.apple.com/source/IOHIDFamily/IOHIDFamily-315.7.16/IOHIDFamily/IOHIDUsageTables.h
--  * (Apple specific) https://opensource.apple.com/source/IOHIDFamily/IOHIDFamily-700/IOHIDFamily/AppleHIDUsageTables.h.auto.html

-- | Map of standard names to Mac Keycodes
keycodeNames :: M.HashMap CoreName Keycode
keycodeNames = M.fromList . flatten $
  -- ###  0x7 => KeyboardorKeypad Page  ###
  [ z 0x7 [0x4..0x1D]  $ map unCore knLetters
  , z 0x7 [0x1E..0x27] numbers
  , z 0x7 [0x28..0x2C] ["ret", "esc", "bspc", "tab", "spc"]
  -- nUS# -> NonUSPound => Non-US # or _
  , z 0x7 [0x2D..0x38] [ "-", "=", "[", "]", "\\", "nUS#"
                       , ";", "'", "`", ",", ".", "/" ]
  , z 0x7 [0x39]       [ "caps" ]
  , z 0x7 [0x3A..0x45] $ map unCore knFKeys
  , z 0x7 [0x46..0x53] [ "sys", "slck", "paus", "ins"
                       , "home", "pgup", "del", "end"
                       , "pgdn", "rght", "left", "down"
                       , "up", "nlck" ]
  , z 0x7 [0x54..0x63] keypadKeys
  -- nUS\\ -> NonUSBackslash => Non-US \ or |
  -- appl -> Application => button for context-menu
  , z 0x7 [0x64..0x67] [ "nUS\\", "appl", "pwr", "kp=" ]
  , z 0x7 [0x68..0x73] fn_13To24
  , z 0x7 [0x74..0x7E] [ "exec", "help", "menu", "sel"
                       , "stop", "agn", "undo"
                       , "cut", "copy", "past", "find" ]
  , z 0x7 [0x7F..0x81] [ "mute", "volu", "vold" ]
  -- Locking {CapsLock, NumLock, ScrollLock}
  , z 0x7 [0x82..0x84] [ "Lcps", "Lnlk", "Lslk" ]
  -- kp=2 -> KpEqualSignAS400 => kp= for AS/400
  , z 0x7 [0x85..0x86] [ "kp,", "kp=2" ]
  , z 0x7 [0x87..0x8F] international_1To9
  , z 0x7 [0x90..0x98] lang_1To9
  , z 0x7 [0x99..0xA4] [ "alte", "sys", "cncl", "clr"
                       , "prio", "ret2", "sep", "out"
                       , "oper", "clr2", "crsl", "exsl" ]
  --
  -- (0x7,0xA5) - (0x7,0xDF) Reserved
  --
  , z 0x7 [0xE0..0xE7] modifiers
  --
  -- (0x7,0xE8) - (0x7,0xFFFF) Reserved
  -- (0x7,0xFFFF) - Keyboard_Reserved
  --

  -- ###  0xC => Consumer Page  ###
  , z 0xC [0x30..0x32] [ "Cpwr", "Crst", "Cslp" ]
  , z 0xC [0xB5..0xB6] [ "nxsg", "prvs" ]
  , z 0xC [0xCD]       [ "plps" ]
  , z 0xC [0xE9..0xEA] [ "Cvlu", "Cvld" ]
  -- Home button used by many iOS/Android keybaords
  , z 0xC [0x223]      ["Chom"]

  -- ###  0xFF01 => AppleVendor Keyboard Page  ###
  , z 0xFF01 [0x1..0x4]   [ "sptl", "dsbd", "func", "lnpd" ]
  , z 0xFF01 [0x10..0x11] [ "expa", "expd" ]
  , z 0xFF01 [0x20..0x21] [ "Vbru", "Vbrd" ]

  -- ###  0xFF => AppleVendor Page Top Case  ###
  , z 0xFF [0x3..0x5] [ "Fn", "brup", "brdn" ]
  -- kbil -> IlluminationToggle
  , z 0xFF [0x7..0x9] [ "kbil", "kbup", "kbdn" ]
  ]
   where
      -- Apple uses "1-9 & 0" order for numbers
      num_1To9 = map tshow [1..9 :: Int]
      numbers = num_1To9 <> ["0"]
      -- first left mods, and then right mods
      modifiers = let mods = [ "ctl", "sft", "alt", "met" ]
                  in prefix "l" mods <> prefix "r" mods
      keypadKeys = prefix "kp" $
        [ "/", "*", "-", "+", "rt" ] <> numbers <> [ "." ]

      fn_13To24 = map (("fn" <>) . tshow) [13..24 :: Int]
      international_1To9 = prefix "int" num_1To9
      lang_1To9 = prefix "lng" num_1To9

      flatten xss = [x | xs <- xss, x <- xs]
      prefix p = map (p <>)
