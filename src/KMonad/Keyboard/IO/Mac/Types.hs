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
type MacKeycode = Word32 -- ^ Type alias for the windows encoded keycode

-- | 'MacKeyEvent' is the C-representation of a a 'KeyEvent' for our Mac API.
--
-- It contains a 'Word8' signifying whether the event was a Press (0) or Release
-- (1), and a 'Word32' (uint32_t) signifying the *Mac keycode*.
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
-- FIXME: There are loads of missing correspondences, mostly for rare-keys. How
-- do these line up? Ideally this mapping would be total.
kcMap :: M.HashMap MacKeycode Keycode
kcMap = M.fromList $
  [ -- (0x01, kHIDUsage_KeyErrorRollOver)
    -- (0x02, KeyPOSTFail)
    -- (0x03, KeyErrorUndefined)
    (0x04, KeyA)
  , (0x05, KeyB)
  , (0x06, KeyC)
  , (0x07, KeyD)
  , (0x08, KeyE)
  , (0x09, KeyF)
  , (0x0A, KeyG)
  , (0x0B, KeyH)
  , (0x0C, KeyI)
  , (0x0D, KeyJ)
  , (0x0E, KeyK)
  , (0x0F, KeyL)
  , (0x10, KeyM)
  , (0x11, KeyN)
  , (0x12, KeyO)
  , (0x13, KeyP)
  , (0x14, KeyQ)
  , (0x15, KeyR)
  , (0x16, KeyS)
  , (0x17, KeyT)
  , (0x18, KeyU)
  , (0x19, KeyV)
  , (0x1A, KeyW)
  , (0x1B, KeyX)
  , (0x1C, KeyY)
  , (0x1D, KeyZ)
  , (0x1E, Key1)
  , (0x1F, Key2)
  , (0x20, Key3)
  , (0x21, Key4)
  , (0x22, Key5)
  , (0x23, Key6)
  , (0x24, Key7)
  , (0x25, Key8)
  , (0x26, Key9)
  , (0x27, Key0)
  , (0x28, KeyEnter)
  , (0x29, KeyEsc)
  , (0x2A, KeyBackspace)
  , (0x2B, KeyTab)
  , (0x2C, KeySpace)
  , (0x2D, KeyMinus)
  , (0x2E, KeyEqual)
  , (0x2F, KeyLeftBrace)
  , (0x30, KeyRightBrace)
  , (0x31, KeyBackslash)
  -- , (0x32, KeyNonUSPound)
  , (0x33, KeySemicolon)
  , (0x34, KeyApostrophe)
  , (0x35, KeyGrave)
  , (0x36, KeyComma)
  , (0x37, KeyDot)
  , (0x38, KeySlash)
  , (0x39, KeyCapsLock)
  , (0x3A, KeyF1)
  , (0x3B, KeyF2)
  , (0x3C, KeyF3)
  , (0x3D, KeyF4)
  , (0x3E, KeyF5)
  , (0x3F, KeyF6)
  , (0x40, KeyF7)
  , (0x41, KeyF8)
  , (0x42, KeyF9)
  , (0x43, KeyF10)
  , (0x44, KeyF11)
  , (0x45, KeyF12)
  , (0x46, KeyPrint)
  , (0x47, KeyScrollLock)
  , (0x48, KeyPause)
  , (0x49, KeyInsert)
  , (0x4A, KeyHome)
  , (0x4B, KeyPageUp)
  , (0x4C, KeyDelete)
  , (0x4D, KeyEnd)
  , (0x4E, KeyPageDown)
  , (0x4F, KeyRight)
  , (0x50, KeyLeft)
  , (0x51, KeyDown)
  , (0x52, KeyUp)
  , (0x53, KeyNumLock)
  , (0x54, KeyKpSlash)
  , (0x55, KeyKpAsterisk)
  , (0x56, KeyKpMinus)
  , (0x57, KeyKpPlus)
  , (0x58, KeyKpenter)
  , (0x59, KeyKp1)
  , (0x5A, KeyKp2)
  , (0x5B, KeyKp3)
  , (0x5C, KeyKp4)
  , (0x5D, KeyKp5)
  , (0x5E, KeyKp6)
  , (0x5F, KeyKp7)
  , (0x60, KeyKp8)
  , (0x61, KeyKp9)
  , (0x62, KeyKp0)
  , (0x63, KeyKpDot)
  -- , (0x64, KeyNonUSBackslash)
  -- , (0x65, KeyApplication)
  , (0x66, KeyPower)
  , (0x67, KeyKpEqual)
  , (0x68, KeyF13)
  , (0x69, KeyF14)
  , (0x6A, KeyF15)
  , (0x6B, KeyF16)
  , (0x6C, KeyF17)
  , (0x6D, KeyF18)
  , (0x6E, KeyF19)
  , (0x6F, KeyF20)
  , (0x70, KeyF21)
  , (0x71, KeyF22)
  , (0x72, KeyF23)
  , (0x73, KeyF24)
  -- , (0x74, KeyExecute)
  , (0x75, KeyHelp)
  , (0x76, KeyMenu)
  -- , (0x77, KeySelect)
  , (0x78, KeyStop)
  , (0x79, KeyAgain)
  , (0x7A, KeyUndo)
  , (0x7B, KeyCut)
  , (0x7C, KeyCopy)
  , (0x7D, KeyPaste)
  , (0x7E, KeyFind)
  , (0x7F, KeyMute)
  , (0x80, KeyVolumeUp)
  , (0x81, KeyVolumeDown)
  -- , (0x82, KeyLockingCapsLock)
  -- , (0x83, KeyLockingNumLock)
  -- , (0x84, KeyLockingScrollLock)
  , (0x85, KeyKpComma)
  -- , (0x86, KeyKpEqualSignAS400)
  -- , (0x87, KeyInternational1)
  -- , (0x88, KeyInternational2)
  -- , (0x89, KeyInternational3)
  -- , (0x8A, KeyInternational4)
  -- , (0x8B, KeyInternational5)
  -- , (0x8C, KeyInternational6)
  -- , (0x8D, KeyInternational7)
  -- , (0x8E, KeyInternational8)
  -- , (0x8F, KeyInternational9)
  -- , (0x90, KeyLANG1)
  -- , (0x91, KeyLANG2)
  -- , (0x92, KeyLANG3)
  -- , (0x93, KeyLANG4)
  -- , (0x94, KeyLANG5)
  -- , (0x95, KeyLANG6)
  -- , (0x96, KeyLANG7)
  -- , (0x97, KeyLANG8)
  -- , (0x98, KeyLANG9)
  -- , (0x99, KeyAlternateErase)
  -- , (0x9A, KeySysReqOrAttention)
  , (0x9B, KeyCancel)
  -- , (0x9C, KeyClear)
  -- , (0x9D, KeyPrior)
  -- , (0x9E, KeyReturn)
  -- , (0x9F, KeySeparator)
  -- , (0xA0, KeyOut)
  -- , (0xA1, KeyOper)
  -- , (0xA2, KeyClearOrAgain)
  -- , (0xA3, KeyCrSelOrProps)
  -- , (0xA4, KeyExSel)
  -- /* 0xA5-0xDF Reserved */
  , (0xE0, KeyLeftCtrl)
  , (0xE1, KeyLeftShift)
  , (0xE2, KeyLeftAlt)
  , (0xE3, KeyLeftMeta)
  , (0xE4, KeyRightCtrl)
  , (0xE5, KeyRightShift)
  , (0xE6, KeyRightAlt)
  , (0xE7, KeyRightMeta)
  -- /* 0xE8-0xFFFF Reserved */
  , (0xFFFF, KeyReserved)
  ]
