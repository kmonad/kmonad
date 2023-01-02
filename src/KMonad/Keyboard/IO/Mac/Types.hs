module KMonad.Keyboard.IO.Mac.Types
  ( MacError(..)
  , MacKeyEvent
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
  | BadMacSwitch     MacSwitch  -- ^ Error interpreting 'MacSwitch'

instance Exception MacError
instance Show MacError where
  show e = case e of
    NoMacKeycodeTo   c -> "Cannot translate to mac keycode: "   <> show c
    NoMacKeycodeFrom i -> "Cannot translate from mac keycode: " <> show i
    BadMacSwitch     s -> "Cannot interpret mac switch: "       <> show s
instance Exception [MacError]

--------------------------------------------------------------------------------
-- $typ

type MacSwitch  = Word64           -- ^ Type alias for the switch value
type MacKeycode = (Word32, Word32) -- ^ Type alias for the Mac keycode

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
  alignment _ = 4
  sizeOf    _ = 16
  peek ptr = do
    s <- peekByteOff ptr 0
    p <- peekByteOff ptr 8
    u <- peekByteOff ptr 12
    return $ MacKeyEvent (s, (p, u))
  poke ptr (MacKeyEvent (s, (p, u))) = do
    pokeByteOff ptr 0 s
    pokeByteOff ptr 8 p
    pokeByteOff ptr 12 u

--------------------------------------------------------------------------------
-- $conv

fromMacSwitch :: MacSwitch -> Maybe Switch
fromMacSwitch s = case s of
  1 -> Just Press
  0 -> Just Release
  _ -> Nothing

toMacSwitch :: Switch -> MacSwitch
toMacSwitch s = if s == Press then 1 else 0

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
  Just c  -> Right $ MacKeyEvent (toMacSwitch (e^.switch), c)
  Nothing -> Left . NoMacKeycodeTo $ e^.keycode

-- | Convert a 'MacKeyEvent' to a 'KeyEvent'
--
-- NOTE: Same limitations as 'toMacKeyEvent' apply
fromMacKeyEvent :: MacKeyEvent -> Maybe (Either [MacError] KeyEvent)
fromMacKeyEvent (MacKeyEvent (s, (p, u)))
  | p == 7 && u <= 0x3    = Nothing
  | p == 7 && u >= 0xFFFF = Nothing
  | otherwise             = case (fromMacKeycode (p, u), fromMacSwitch s) of
      (Just c', Just s') -> Just (Right $ mkKeyEvent s' c')
      (Just _, Nothing)  -> Just (Left [BadMacSwitch s])
      (Nothing, Just _)  -> Just (Left [NoMacKeycodeFrom (p,u)])
      (Nothing, Nothing) -> Just (Left [BadMacSwitch s, NoMacKeycodeFrom (p,u)])

--------------------------------------------------------------------------------
-- $kc

-- | Mac does not use the same keycodes as Linux, so we need to translate.
--
-- See https://opensource.apple.com/source/IOHIDFamily/IOHIDFamily-315.7.16/IOHIDFamily/IOHIDUsageTables.h
-- See https://opensource.apple.com/source/IOHIDFamily/IOHIDFamily-700/IOHIDFamily/AppleHIDUsageTables.h.auto.html
kcMap :: M.HashMap MacKeycode Keycode
kcMap = M.fromList $
  [ ((0x7,0x4), KeyA)
  , ((0x7,0x5), KeyB)
  , ((0x7,0x6), KeyC)
  , ((0x7,0x7), KeyD)
  , ((0x7,0x8), KeyE)
  , ((0x7,0x9), KeyF)
  , ((0x7,0xA), KeyG)
  , ((0x7,0xB), KeyH)
  , ((0x7,0xC), KeyI)
  , ((0x7,0xD), KeyJ)
  , ((0x7,0xE), KeyK)
  , ((0x7,0xF), KeyL)
  , ((0x7,0x10), KeyM)
  , ((0x7,0x11), KeyN)
  , ((0x7,0x12), KeyO)
  , ((0x7,0x13), KeyP)
  , ((0x7,0x14), KeyQ)
  , ((0x7,0x15), KeyR)
  , ((0x7,0x16), KeyS)
  , ((0x7,0x17), KeyT)
  , ((0x7,0x18), KeyU)
  , ((0x7,0x19), KeyV)
  , ((0x7,0x1A), KeyW)
  , ((0x7,0x1B), KeyX)
  , ((0x7,0x1C), KeyY)
  , ((0x7,0x1D), KeyZ)
  , ((0x7,0x1E), Key1)
  , ((0x7,0x1F), Key2)
  , ((0x7,0x20), Key3)
  , ((0x7,0x21), Key4)
  , ((0x7,0x22), Key5)
  , ((0x7,0x23), Key6)
  , ((0x7,0x24), Key7)
  , ((0x7,0x25), Key8)
  , ((0x7,0x26), Key9)
  , ((0x7,0x27), Key0)
  , ((0x7,0x28), KeyEnter)
  , ((0x7,0x29), KeyEsc)
  , ((0x7,0x2A), KeyBackspace)
  , ((0x7,0x2B), KeyTab)
  , ((0x7,0x2C), KeySpace)
  , ((0x7,0x2D), KeyMinus)
  , ((0x7,0x2E), KeyEqual)
  , ((0x7,0x2F), KeyLeftBrace)
  , ((0x7,0x30), KeyRightBrace)
  , ((0x7,0x31), KeyBackslash)
  -- , ((0x7,0x32), KeyNonUSPound)
  , ((0x7,0x33), KeySemicolon)
  , ((0x7,0x34), KeyApostrophe)
  , ((0x7,0x35), KeyGrave)
  , ((0x7,0x36), KeyComma)
  , ((0x7,0x37), KeyDot)
  , ((0x7,0x38), KeySlash)
  , ((0x7,0x39), KeyCapsLock)
  , ((0x7,0x3A), KeyF1)
  , ((0x7,0x3B), KeyF2)
  , ((0x7,0x3C), KeyF3)
  , ((0x7,0x3D), KeyF4)
  , ((0x7,0x3E), KeyF5)
  , ((0x7,0x3F), KeyF6)
  , ((0x7,0x40), KeyF7)
  , ((0x7,0x41), KeyF8)
  , ((0x7,0x42), KeyF9)
  , ((0x7,0x43), KeyF10)
  , ((0x7,0x44), KeyF11)
  , ((0x7,0x45), KeyF12)
  , ((0x7,0x46), KeyPrint)
  , ((0x7,0x47), KeyScrollLock)
  , ((0x7,0x48), KeyPause)
  , ((0x7,0x49), KeyInsert)
  , ((0x7,0x4A), KeyHome)
  , ((0x7,0x4B), KeyPageUp)
  , ((0x7,0x4C), KeyDelete)
  , ((0x7,0x4D), KeyEnd)
  , ((0x7,0x4E), KeyPageDown)
  , ((0x7,0x4F), KeyRight)
  , ((0x7,0x50), KeyLeft)
  , ((0x7,0x51), KeyDown)
  , ((0x7,0x52), KeyUp)
  , ((0x7,0x53), KeyNumLock)
  , ((0x7,0x54), KeyKpSlash)
  , ((0x7,0x55), KeyKpAsterisk)
  , ((0x7,0x56), KeyKpMinus)
  , ((0x7,0x57), KeyKpPlus)
  , ((0x7,0x58), KeyKpEnter)
  , ((0x7,0x59), KeyKp1)
  , ((0x7,0x5A), KeyKp2)
  , ((0x7,0x5B), KeyKp3)
  , ((0x7,0x5C), KeyKp4)
  , ((0x7,0x5D), KeyKp5)
  , ((0x7,0x5E), KeyKp6)
  , ((0x7,0x5F), KeyKp7)
  , ((0x7,0x60), KeyKp8)
  , ((0x7,0x61), KeyKp9)
  , ((0x7,0x62), KeyKp0)
  , ((0x7,0x63), KeyKpDot)
  -- , ((0x7,0x64), KeyNonUSBackslash)
  -- , ((0x7,0x65), KeyApplication)
  , ((0x7,0x66), KeyPower)
  , ((0x7,0x67), KeyKpEqual)
  , ((0x7,0x68), KeyF13)
  , ((0x7,0x69), KeyF14)
  , ((0x7,0x6A), KeyF15)
  , ((0x7,0x6B), KeyF16)
  , ((0x7,0x6C), KeyF17)
  , ((0x7,0x6D), KeyF18)
  , ((0x7,0x6E), KeyF19)
  , ((0x7,0x6F), KeyF20)
  , ((0x7,0x70), KeyF21)
  , ((0x7,0x71), KeyF22)
  , ((0x7,0x72), KeyF23)
  , ((0x7,0x73), KeyF24)
  -- , ((0x7,0x74), KeyExecute)
  , ((0x7,0x75), KeyHelp)
  , ((0x7,0x76), KeyMenu)
  -- , ((0x7,0x77), KeySelect)
  , ((0x7,0x78), KeyStop)
  , ((0x7,0x79), KeyAgain)
  , ((0x7,0x7A), KeyUndo)
  , ((0x7,0x7B), KeyCut)
  , ((0x7,0x7C), KeyCopy)
  , ((0x7,0x7D), KeyPaste)
  , ((0x7,0x7E), KeyFind)
  , ((0x7,0x7F), KeyMute)
  , ((0x7,0x80), KeyVolumeUp)
  , ((0x7,0x81), KeyVolumeDown)
  -- , ((0x7,0x82), KeyLockingCapsLock)
  -- , ((0x7,0x83), KeyLockingNumLock)
  -- , ((0x7,0x84), KeyLockingScrollLock)
  , ((0x7,0x85), KeyKpComma)
  -- , ((0x7,0x86), KeyKpEqualSignAS400)
  -- , ((0x7,0x87), KeyInternational1)
  -- , ((0x7,0x88), KeyInternational2)
  -- , ((0x7,0x89), KeyInternational3)
  -- , ((0x7,0x8A), KeyInternational4)
  -- , ((0x7,0x8B), KeyInternational5)
  -- , ((0x7,0x8C), KeyInternational6)
  -- , ((0x7,0x8D), KeyInternational7)
  -- , ((0x7,0x8E), KeyInternational8)
  -- , ((0x7,0x8F), KeyInternational9)
  -- , ((0x7,0x90), KeyLANG1)
  -- , ((0x7,0x91), KeyLANG2)
  -- , ((0x7,0x92), KeyLANG3)
  -- , ((0x7,0x93), KeyLANG4)
  -- , ((0x7,0x94), KeyLANG5)
  -- , ((0x7,0x95), KeyLANG6)
  -- , ((0x7,0x96), KeyLANG7)
  -- , ((0x7,0x97), KeyLANG8)
  -- , ((0x7,0x98), KeyLANG9)
  -- , ((0x7,0x99), KeyAlternateErase)
  -- , ((0x7,0x9A), KeySysReqOrAttention)
  , ((0x7,0x9B), KeyCancel)
  -- , ((0x7,0x9C), KeyClear)
  -- , ((0x7,0x9D), KeyPrior)
  -- , ((0x7,0x9E), KeyReturn)
  -- , ((0x7,0x9F), KeySeparator)
  -- , ((0x7,0xA0), KeyOut)
  -- , ((0x7,0xA1), KeyOper)
  -- , ((0x7,0xA2), KeyClearOrAgain)
  -- , ((0x7,0xA3), KeyCrSelOrProps)
  -- , ((0x7,0xA4), KeyExSel)
  -- (0x7,0xA5) - (0x7,0xDF) Reserved
  , ((0x7,0xE0), KeyLeftCtrl)
  , ((0x7,0xE1), KeyLeftShift)
  , ((0x7,0xE2), KeyLeftAlt)
  , ((0x7,0xE3), KeyLeftMeta)
  , ((0x7,0xE4), KeyRightCtrl)
  , ((0x7,0xE5), KeyRightShift)
  , ((0x7,0xE6), KeyRightAlt)
  , ((0x7,0xE7), KeyRightMeta)
  -- (0x7,0xE8) - (0x7,0xFFFF) Reserved
  , ((0xC,0xB5), KeyNextSong)
  , ((0xC,0xB6), KeyPreviousSong)
  , ((0xC,0xCD), KeyPlayPause)
  , ((0xC,0xCF), KeyDictation)
  , ((0xFF,0x3), KeyFn)
  , ((0xFF,0x4), KeyBrightnessUp)
  , ((0xFF,0x5), KeyBrightnessDown)
  , ((0xFF,0x8), KeyKbdIllumUp)
  , ((0xFF,0x9), KeyKbdIllumDown)
  , ((0xFF01,0x1), KeySpotlight)
  , ((0xFF01,0x4), KeyLaunchpad)
  , ((0xFF01,0x10), KeyMissionCtrl)
  ]
