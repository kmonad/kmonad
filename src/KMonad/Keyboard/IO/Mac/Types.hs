module KMonad.Keyboard.IO.Mac.Types
  ( MacError(..)
  , MacKeyEvent
  , mkMacKeyEvent
  , toMacKeyEvent
  , fromMacKeyEvent
  )

where

import KMonad.Prelude

import Foreign.Storable
import KMonad.Keyboard

import qualified RIO.HashMap as M


----------------------------------------------------------------------------
-- $err

-- | Everything that can go wrong with Mac Key-IO
data MacError
  = NoMacKeycodeTo   Keycode    -- ^ Error translating to 'MacKeycode'
  | NoMacKeycodeFrom MacKeycode -- ^ Error translating from 'MacKeycode'

instance Exception MacError
instance Show MacError where
  show e = case e of
    NoMacKeycodeTo   c -> "Cannot translate to mac keycode: "   <> show c
    NoMacKeycodeFrom i -> "Cannot translate from mac keycode: " <> show i

--------------------------------------------------------------------------------
-- $typ

type MacSwitch  = Word8  -- ^ Type alias for the switch value
type MacKeycode = Word32 -- ^ Type alias for the Mac keycode

-- | 'MacKeyEvent' is the C-representation of a a 'KeyEvent' for our Mac API.
--
-- It contains a 'Word8' signifying whether the event was a Press (0)
-- or Release (1), and a 'Word32' (uint32_t) signifying the Mac
-- keycode (the upper 16 bits represent the IOKit usage page, and the
-- lower 16 bits represent the IOKit usage).
--
-- NOTE: Mac and Linux keycodes do not line up. Internally we use Linux
-- Keycodes for everything, we translate at the KeyIO stage (here).
newtype MacKeyEvent = MacKeyEvent (MacSwitch, MacKeycode)
  deriving (Eq, Ord, Show)

-- | This lets us send 'MacKeyEvent's between Haskell and C.
instance Storable MacKeyEvent where
  alignment _ = 4 -- lowest common denominator of: 1 4
  sizeOf    _ = 8 -- (1 + 3-padding) + 4
  peek ptr = do
    s <- peekByteOff ptr 0
    c <- peekByteOff ptr 4
    return $ MacKeyEvent (s, c)
  poke ptr (MacKeyEvent (s, c)) = do
    pokeByteOff ptr 0 s
    pokeByteOff ptr 4 c

mkMacKeyEvent :: MacSwitch -> MacKeycode -> MacKeyEvent
mkMacKeyEvent s e = MacKeyEvent (s, e)

--------------------------------------------------------------------------------
-- $conv

-- | Convert between 'MacSwitch' and 'Switch' representations.
--
-- NOTE: Although 'MacSwitch' could theoretically be something besides 0 or 1,
-- practically it can't, because those are the only values the API generates,
-- guaranteed.
_MacSwitch :: Iso' MacSwitch Switch
_MacSwitch = iso to' from'
  where
    to' w   = if w == 0 then Press else Release
    from' s = if s == Press then 0 else 1

-- | Lookup the corresponding 'Keycode' for this 'MacKeycode'
fromMacKeycode :: MacKeycode -> Maybe Keycode
fromMacKeycode = flip M.lookup kcMap

-- | Lookup the correspondig 'MacKeycode' for this 'Keycode'
toMacKeycode :: Keycode -> Maybe MacKeycode
toMacKeycode = flip M.lookup revMap
  where revMap = M.fromList $ (M.toList kcMap) ^.. folded . swapped

-- | Convert a 'KeyEvent' to a 'MacKeyEvent'
--
-- NOTE: Mac keycodes are different, and I am not confident I have full
-- coverage, therefore this conversion is not total. We are going to leave this
-- error-handling in until we are sure this is covered well. Once it lines up
-- perfectly, this is essentially an Iso.
toMacKeyEvent :: KeyEvent -> Either MacError MacKeyEvent
toMacKeyEvent e = case toMacKeycode $ e^.keycode of
  Just c  -> Right $ MacKeyEvent (e^.switch.from _MacSwitch, c)
  Nothing -> Left . NoMacKeycodeTo $ e^.keycode

-- | Convert a 'MacKeyEvent' to a 'KeyEvent'
--
-- NOTE: Same limitations as 'toMacKeyEvent' apply
fromMacKeyEvent :: MacKeyEvent -> Either MacError KeyEvent
fromMacKeyEvent (MacKeyEvent (s, c)) = case fromMacKeycode c of
  Just c' -> Right $ mkKeyEvent (s^._MacSwitch) c'
  Nothing -> Left . NoMacKeycodeFrom $ c


--------------------------------------------------------------------------------
-- $kc

-- | Mac does not use the same keycodes as Linux, so we need to translate.
--
-- See https://opensource.apple.com/source/IOHIDFamily/IOHIDFamily-315.7.16/IOHIDFamily/IOHIDUsageTables.h
-- See https://opensource.apple.com/source/IOHIDFamily/IOHIDFamily-700/IOHIDFamily/AppleHIDUsageTables.h.auto.html
kcMap :: M.HashMap MacKeycode Keycode
kcMap = M.fromList $
  [ (0x00070000, KeyError) -- There's no documentation on this error code, but
                           -- I've seen it sent when the rollover is exceeded on
                           -- my macbook internal keyboard
  , (0x00070001, KeyError) -- kHIDUsage_KeyErrorRollOver
  , (0x00070002, KeyError) -- kHIDUsage_KeyPOSTFail
  , (0x00070003, KeyError) -- kHIDUsage_Undefined
  , (0x00070004, KeyA)
  , (0x00070005, KeyB)
  , (0x00070006, KeyC)
  , (0x00070007, KeyD)
  , (0x00070008, KeyE)
  , (0x00070009, KeyF)
  , (0x0007000A, KeyG)
  , (0x0007000B, KeyH)
  , (0x0007000C, KeyI)
  , (0x0007000D, KeyJ)
  , (0x0007000E, KeyK)
  , (0x0007000F, KeyL)
  , (0x00070010, KeyM)
  , (0x00070011, KeyN)
  , (0x00070012, KeyO)
  , (0x00070013, KeyP)
  , (0x00070014, KeyQ)
  , (0x00070015, KeyR)
  , (0x00070016, KeyS)
  , (0x00070017, KeyT)
  , (0x00070018, KeyU)
  , (0x00070019, KeyV)
  , (0x0007001A, KeyW)
  , (0x0007001B, KeyX)
  , (0x0007001C, KeyY)
  , (0x0007001D, KeyZ)
  , (0x0007001E, Key1)
  , (0x0007001F, Key2)
  , (0x00070020, Key3)
  , (0x00070021, Key4)
  , (0x00070022, Key5)
  , (0x00070023, Key6)
  , (0x00070024, Key7)
  , (0x00070025, Key8)
  , (0x00070026, Key9)
  , (0x00070027, Key0)
  , (0x00070028, KeyEnter)
  , (0x00070029, KeyEsc)
  , (0x0007002A, KeyBackspace)
  , (0x0007002B, KeyTab)
  , (0x0007002C, KeySpace)
  , (0x0007002D, KeyMinus)
  , (0x0007002E, KeyEqual)
  , (0x0007002F, KeyLeftBrace)
  , (0x00070030, KeyRightBrace)
  , (0x00070031, KeyBackslash)
  -- , (0x00070032, KeyNonUSPound)
  , (0x00070033, KeySemicolon)
  , (0x00070034, KeyApostrophe)
  , (0x00070035, KeyGrave)
  , (0x00070036, KeyComma)
  , (0x00070037, KeyDot)
  , (0x00070038, KeySlash)
  , (0x00070039, KeyCapsLock)
  , (0x0007003A, KeyF1)
  , (0x0007003B, KeyF2)
  , (0x0007003C, KeyF3)
  , (0x0007003D, KeyF4)
  , (0x0007003E, KeyF5)
  , (0x0007003F, KeyF6)
  , (0x00070040, KeyF7)
  , (0x00070041, KeyF8)
  , (0x00070042, KeyF9)
  , (0x00070043, KeyF10)
  , (0x00070044, KeyF11)
  , (0x00070045, KeyF12)
  , (0x00070046, KeyPrint)
  , (0x00070047, KeyScrollLock)
  , (0x00070048, KeyPause)
  , (0x00070049, KeyInsert)
  , (0x0007004A, KeyHome)
  , (0x0007004B, KeyPageUp)
  , (0x0007004C, KeyDelete)
  , (0x0007004D, KeyEnd)
  , (0x0007004E, KeyPageDown)
  , (0x0007004F, KeyRight)
  , (0x00070050, KeyLeft)
  , (0x00070051, KeyDown)
  , (0x00070052, KeyUp)
  , (0x00070053, KeyNumLock)
  , (0x00070054, KeyKpSlash)
  , (0x00070055, KeyKpAsterisk)
  , (0x00070056, KeyKpMinus)
  , (0x00070057, KeyKpPlus)
  , (0x00070058, KeyKpEnter)
  , (0x00070059, KeyKp1)
  , (0x0007005A, KeyKp2)
  , (0x0007005B, KeyKp3)
  , (0x0007005C, KeyKp4)
  , (0x0007005D, KeyKp5)
  , (0x0007005E, KeyKp6)
  , (0x0007005F, KeyKp7)
  , (0x00070060, KeyKp8)
  , (0x00070061, KeyKp9)
  , (0x00070062, KeyKp0)
  , (0x00070063, KeyKpDot)
  -- , (0x00070064, KeyNonUSBackslash)
  -- , (0x00070065, KeyApplication)
  , (0x00070066, KeyPower)
  , (0x00070067, KeyKpEqual)
  , (0x00070068, KeyF13)
  , (0x00070069, KeyF14)
  , (0x0007006A, KeyF15)
  , (0x0007006B, KeyF16)
  , (0x0007006C, KeyF17)
  , (0x0007006D, KeyF18)
  , (0x0007006E, KeyF19)
  , (0x0007006F, KeyF20)
  , (0x00070070, KeyF21)
  , (0x00070071, KeyF22)
  , (0x00070072, KeyF23)
  , (0x00070073, KeyF24)
  -- , (0x00070074, KeyExecute)
  , (0x00070075, KeyHelp)
  , (0x00070076, KeyMenu)
  -- , (0x00070077, KeySelect)
  , (0x00070078, KeyStop)
  , (0x00070079, KeyAgain)
  , (0x0007007A, KeyUndo)
  , (0x0007007B, KeyCut)
  , (0x0007007C, KeyCopy)
  , (0x0007007D, KeyPaste)
  , (0x0007007E, KeyFind)
  , (0x0007007F, KeyMute)
  , (0x00070080, KeyVolumeUp)
  , (0x00070081, KeyVolumeDown)
  -- , (0x00070082, KeyLockingCapsLock)
  -- , (0x00070083, KeyLockingNumLock)
  -- , (0x00070084, KeyLockingScrollLock)
  , (0x00070085, KeyKpComma)
  -- , (0x00070086, KeyKpEqualSignAS400)
  -- , (0x00070087, KeyInternational1)
  -- , (0x00070088, KeyInternational2)
  -- , (0x00070089, KeyInternational3)
  -- , (0x0007008A, KeyInternational4)
  -- , (0x0007008B, KeyInternational5)
  -- , (0x0007008C, KeyInternational6)
  -- , (0x0007008D, KeyInternational7)
  -- , (0x0007008E, KeyInternational8)
  -- , (0x0007008F, KeyInternational9)
  -- , (0x00070090, KeyLANG1)
  -- , (0x00070091, KeyLANG2)
  -- , (0x00070092, KeyLANG3)
  -- , (0x00070093, KeyLANG4)
  -- , (0x00070094, KeyLANG5)
  -- , (0x00070095, KeyLANG6)
  -- , (0x00070096, KeyLANG7)
  -- , (0x00070097, KeyLANG8)
  -- , (0x00070098, KeyLANG9)
  -- , (0x00070099, KeyAlternateErase)
  -- , (0x0007009A, KeySysReqOrAttention)
  , (0x0007009B, KeyCancel)
  -- , (0x0007009C, KeyClear)
  -- , (0x0007009D, KeyPrior)
  -- , (0x0007009E, KeyReturn)
  -- , (0x0007009F, KeySeparator)
  -- , (0x000700A0, KeyOut)
  -- , (0x000700A1, KeyOper)
  -- , (0x000700A2, KeyClearOrAgain)
  -- , (0x000700A3, KeyCrSelOrProps)
  -- , (0x000700A4, KeyExSel)
  -- /* 0x000700A5-0x000700DF Reserved */
  , (0x000700E0, KeyLeftCtrl)
  , (0x000700E1, KeyLeftShift)
  , (0x000700E2, KeyLeftAlt)
  , (0x000700E3, KeyLeftMeta)
  , (0x000700E4, KeyRightCtrl)
  , (0x000700E5, KeyRightShift)
  , (0x000700E6, KeyRightAlt)
  , (0x000700E7, KeyRightMeta)
  -- /* 0x000700E8-0x0007FFFF Reserved */
  , (0x0007FFFF, KeyReserved)
  , (0x000C00B5, KeyNextSong)
  , (0x000C00B6, KeyPreviousSong)
  , (0x000C00CD, KeyPlayPause)
  , (0x00FF0003, KeyFn)
  , (0x00FF0004, KeyBrightnessUp)
  , (0x00FF0005, KeyBrightnessDown)
  , (0x00FF0008, KeyBacklightUp)
  , (0x00FF0009, KeyBacklightDown)
  , (0xFF010004, KeyLaunchpad)
  , (0xFF010010, KeyMissionCtrl)
  ]
