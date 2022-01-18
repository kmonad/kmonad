{-|
Module      : KMonad.Keyboard.IO.Windows.Types
Description : The Windows-specific representation of KeyEvent.
Copyright   : (c) David Janssen, 2019
License     : MIT
Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : portable

NOTE: The representation here lines up with the @keyio_win.c@ module, not with
Windows in general. There is some translation happening in the c-code.

-}
module KMonad.Keyboard.IO.Windows.Types
  ( WinError(..)
  , WinKeyEvent
  , mkWinKeyEvent
  , toWinKeyEvent
  , fromWinKeyEvent
  )

where

import KMonad.Prelude

import Foreign.Storable
import KMonad.Keyboard

import qualified RIO.HashMap as M


----------------------------------------------------------------------------
-- $err

-- | Everything that can go wrong with Windows Key-IO
data WinError
  = NoWinKeycodeTo   Keycode    -- ^ Error translating to 'WinKeycode'
  | NoWinKeycodeFrom WinKeycode -- ^ Error translating from 'WinKeycode'

instance Exception WinError
instance Show WinError where
  show e = case e of
    NoWinKeycodeTo   c -> "Cannot translate to windows keycode: "   <> show c
    NoWinKeycodeFrom i -> "Cannot translate from windows keycode: " <> show i

--------------------------------------------------------------------------------
-- $typ

type WinSwitch  = Word8  -- ^ Type alias for the switch value
type WinKeycode = Word32 -- ^ Type alias for the windows encoded keycode

-- | 'WinKeyEvent' is the C-representation of a a 'KeyEvent' for our Windows API.
--
-- It contains a 'Word8' signifying whether the event was a Press (0) or Release
-- (1), and a 'Word32' (Windows @DWORD@) signifying the *Windows keycode*.
--
-- NOTE: Windows and Linux keycodes do not line up. Internally we use Linux
-- Keycodes for everything, we translate at the KeyIO stage (here).
newtype WinKeyEvent = WinKeyEvent (WinSwitch, WinKeycode)
  deriving (Eq, Ord, Show)

-- | This lets us send 'WinKeyEvent's between Haskell and C.
instance Storable WinKeyEvent where
  alignment _ = 4 -- lowest common denominator of: 1 4
  sizeOf    _ = 8 -- (1 + 3-padding) + 4
  peek ptr = do
    s <- peekByteOff ptr 0
    c <- peekByteOff ptr 4
    return $ WinKeyEvent (s, c)
  poke ptr (WinKeyEvent (s, c)) = do
    pokeByteOff ptr 0 s
    pokeByteOff ptr 4 c

mkWinKeyEvent :: WinSwitch -> WinKeycode -> WinKeyEvent
mkWinKeyEvent s e = WinKeyEvent (s, e)

--------------------------------------------------------------------------------
-- $conv

-- | Convert between 'WinSwitch' and 'Switch' representations.
--
-- NOTE: Although 'WinSwitch' could theoretically be something besides 0 or 1,
-- practically it can't, because those are the only values the API generates,
-- guaranteed.
_WinSwitch :: Iso' WinSwitch Switch
_WinSwitch = iso to' from'
  where
    to' w   = if w == 0 then Press else Release
    from' s = if s == Press then 0 else 1

-- | Lookup the corresponding 'Keycode' for this 'WinKeycode'
fromWinKeycode :: WinKeycode -> Maybe Keycode
fromWinKeycode = flip M.lookup winCodeToKeyCode

-- | Lookup the correspondig 'WinKeycode' for this 'Keycode'
toWinKeycode :: Keycode -> Maybe WinKeycode
toWinKeycode = flip M.lookup keyCodeToWinCode

-- | Convert a 'KeyEvent' to a 'WinKeyEvent'
--
-- NOTE: Windows keycodes are different, and I am not confident I have full
-- coverage, therefore this conversion is not total. We are going to leave this
-- error-handling in until we are sure this is covered well. Once it lines up
-- perfectly, this is essentially an Iso.
toWinKeyEvent :: KeyEvent -> Either WinError WinKeyEvent
toWinKeyEvent e = case toWinKeycode $ e^.keycode of
  Just c  -> Right $ WinKeyEvent (e^.switch.from _WinSwitch, c)
  Nothing -> Left . NoWinKeycodeTo $ e^.keycode

-- | Convert a 'WinKeyEvent' to a 'KeyEvent'
--
-- NOTE: Same limitations as 'toWinKeyEvent' apply
fromWinKeyEvent :: WinKeyEvent -> Either WinError KeyEvent
fromWinKeyEvent (WinKeyEvent (s, c)) = case fromWinKeycode c of
  Just c' -> Right $ mkKeyEvent (s^._WinSwitch) c'
  Nothing -> Left . NoWinKeycodeFrom $ c


--------------------------------------------------------------------------------
-- $kc

-- | Translate a virtual-key code from Windows into a suitable KMonad KeyCode
--
-- FIXME: There are loads of missing correspondences, mostly for rare-keys. How
-- do these line up? Ideally this mapping would be total.
winCodeToKeyCode :: M.HashMap WinKeycode Keycode
winCodeToKeyCode = M.fromList $
  [ (0x00, Missing254)     -- Not documented, but happens often. Why??
  -- , (0x01, ???)         -- Defined as VK_LBUTTON
  -- , (0x02, ???)         -- Defined as VK_RBUTTON
  , (0x03, KeyCancel)
  -- , (0x04, ???)         -- Defined as VK_MBUTTON
  -- , (0x05, ???)         -- Defined as VK_XBUTTON1
  -- , (0x06, ???)         -- Defined as VK_XBUTTON2
  , (0x08, KeyBackspace)
  , (0x09, KeyTab)
  , (0x0C, KeyDelete)      -- Defined as VK_CLEAR
  , (0x0D, KeyEnter)
  , (0x10, KeyLeftShift)   -- No 'sidedness'??
  , (0x11, KeyLeftCtrl)    -- No 'sidedness'??
  , (0x12, KeyLeftAlt)     -- No 'sidedness'??
  , (0x13, KeyPause)
  , (0x14, KeyCapsLock)
  , (0x15, KeyKatakana)    -- Also: KeyHangul
  -- , (0x16, ???)            -- Defined as VK_IME_ON
  -- , (0x17, ???)            -- Defined as VK_JUNJA
  -- , (0x18, ???)            -- Defined as VK_FINAL
  , (0x19, KeyHanja)
  -- , (0x1A, ???)            -- Defined as VK_IME_OFF
  , (0x1B, KeyEsc)
  , (0x1C, KeyHenkan)         -- Defined as VK_CONVERT
  , (0x1D, KeyMuhenkan)       -- Defined as VK_NONCONVERT
  -- , (0x1E, ???)            -- Defined as VK_ACCEPT
  -- , (0x1F, ???)            -- Defined as VK_MODECHANGE
  , (0x20, KeySpace)
  , (0x21, KeyPageUp)
  , (0x22, KeyPageDown)
  , (0x23, KeyEnd)
  , (0x24, KeyHome)
  , (0x25, KeyLeft)
  , (0x26, KeyUp)
  , (0x27, KeyRight)
  , (0x28, KeyDown)
  -- , (0x29, ???)            -- Defined as VK_SELECT
  , (0x2A, KeyPrint)
  -- , (0x2B, ???)            -- Defined as VK_EXECUTE
  , (0x2C, KeyPrint)          -- Defined as VK_PRINT_SCREEN
  , (0x2D, KeyInsert)
  , (0x2E, KeyDelete)
  , (0x2F, KeyHelp)
  , (0x30, Key0)
  , (0x31, Key1)
  , (0x32, Key2)
  , (0x33, Key3)
  , (0x34, Key4)
  , (0x35, Key5)
  , (0x36, Key6)
  , (0x37, Key7)
  , (0x38, Key8)
  , (0x39, Key9)
  , (0x41, KeyA)
  , (0x42, KeyB)
  , (0x43, KeyC)
  , (0x44, KeyD)
  , (0x45, KeyE)
  , (0x46, KeyF)
  , (0x47, KeyG)
  , (0x48, KeyH)
  , (0x49, KeyI)
  , (0x4A, KeyJ)
  , (0x4B, KeyK)
  , (0x4C, KeyL)
  , (0x4D, KeyM)
  , (0x4E, KeyN)
  , (0x4F, KeyO)
  , (0x50, KeyP)
  , (0x51, KeyQ)
  , (0x52, KeyR)
  , (0x53, KeyS)
  , (0x54, KeyT)
  , (0x55, KeyU)
  , (0x56, KeyV)
  , (0x57, KeyW)
  , (0x58, KeyX)
  , (0x59, KeyY)
  , (0x5A, KeyZ)
  , (0x5B, KeyLeftMeta)             -- Defined as Left Windows key (Natural Keyboard)
  , (0x5C, KeyRightMeta)             -- Defined as Right Windows key (Natural Keyboard)
  , (0x5D, KeyCompose)             -- Defined as Applications key (Natural Keyboard)
  , (0x5F, KeySleep)
  , (0x60, KeyKp0)
  , (0x61, KeyKp1)
  , (0x62, KeyKp2)
  , (0x63, KeyKp3)
  , (0x64, KeyKp4)
  , (0x65, KeyKp5)
  , (0x66, KeyKp6)
  , (0x67, KeyKp7)
  , (0x68, KeyKp8)
  , (0x69, KeyKp9)
  , (0x6A, KeyKpAsterisk)
  , (0x6B, KeyKpPlus)
  -- , (0x6C, KeyKpDot)        -- Defined as VK_SEPARATOR
  , (0x6D, KeyKpMinus)
  , (0x6E, KeyKpDot)
  , (0x6F, KeyKpSlash)
  , (0x70, KeyF1)
  , (0x71, KeyF2)
  , (0x72, KeyF3)
  , (0x73, KeyF4)
  , (0x74, KeyF5)
  , (0x75, KeyF6)
  , (0x76, KeyF7)
  , (0x77, KeyF8)
  , (0x78, KeyF9)
  , (0x79, KeyF10)
  , (0x7A, KeyF11)
  , (0x7B, KeyF12)
  , (0x7C, KeyF13)
  , (0x7D, KeyF14)
  , (0x7E, KeyF15)
  , (0x7F, KeyF16)
  , (0x80, KeyF17)
  , (0x81, KeyF18)
  , (0x82, KeyF19)
  , (0x83, KeyF20)
  , (0x84, KeyF21)
  , (0x85, KeyF22)
  , (0x86, KeyF23)
  , (0x87, KeyF24)
  , (0x90, KeyNumLock)
  , (0x91, KeyScrollLock)
  , (0xA0, KeyLeftShift)
  , (0xA1, KeyRightShift)
  , (0xA2, KeyLeftCtrl)
  , (0xA3, KeyRightCtrl)
  , (0xA4, KeyLeftAlt)
  , (0xA5, KeyRightAlt)
  , (0xA6, KeyBack)
  , (0xA7, KeyForward)
  , (0xA8, KeyRefresh)
  , (0xA9, KeyStop)
  , (0xAA, KeySearch)
  -- , (0xAB, ???)             -- Defined as VK_BROWSER_FAVORITES
  , (0xAC, KeyHomepage)
  , (0xAD, KeyMute)
  , (0xAE, KeyVolumeDown)
  , (0xAF, KeyVolumeUp)
  , (0xB0, KeyNextSong)
  , (0xB1, KeyPreviousSong)
  , (0xB2, KeyStopCd)
  , (0xB3, KeyPlayPause)
  , (0xB4, KeyMail)
  , (0xB5, KeyMedia)
  , (0xB6, KeyProg1)           -- Defined as VK_LAUNCH_APP1
  , (0xB7, KeyProg2)           -- Defined as VK_LAUNCH_APP2
  , (0xBA, KeySemicolon)    -- Defined as VK_OEM_1
  , (0xBB, KeyEqual)        -- Defined as VK_OEM_PLUS
  , (0xBC, KeyComma)        -- Defined as VK_OEM_COMMA
  , (0xBD, KeyMinus)        -- Defined as VK_OEM_MINUS
  , (0xBE, KeyDot)          -- Defined as VK_OEM_PERIOD
  , (0xBF, KeySlash)        -- Defined as VK_OEM_2
  , (0xC0, KeyGrave)        -- Defined as VK_OEM_3
  , (0xDB, KeyLeftBrace)    -- Defined as VK_OEM_4
  , (0xDC, KeyBackslash)    -- Defined as VK_OEM_5
  , (0xDD, KeyRightBrace)   -- Defined as VK_OEM_6
  , (0xDE, KeyApostrophe)   -- Defined as VK_OEM_7
  -- , (0xDF, ???)             -- Defined ask VK_OEM_8
  -- , (0xE1, ???)             -- Defined as `OEM specific`
  , (0xE2, Key102nd)
  -- , (0xE3, ???)             -- Defined as `OEM specific`
  -- , (0xE4, ???)             -- Defined as `OEM specific`
  -- , (0xE5, ???)             -- Defined as VK_PROCESSKEY
  -- , (0xE6, ???)             -- Defined as `OEM specific`
  -- , (0xE7, ???)             -- Defined as VK_PACKET
  -- , (0xE9, ???)             -- Defined as `OEM specific`
  -- , (0xEA, ???)             -- Defined as `OEM specific`
  -- , (0xEB, ???)             -- Defined as `OEM specific`
  -- , (0xEC, ???)             -- Defined as `OEM specific`
  -- , (0xED, ???)             -- Defined as `OEM specific`
  -- , (0xEE, ???)             -- Defined as `OEM specific`
  -- , (0xEF, ???)             -- Defined as `OEM specific`
  -- , (0xF0, ???)             -- Defined as `OEM specific`
  -- , (0xF1, ???)             -- Defined as `OEM specific`
  -- , (0xF2, ???)             -- Defined as `OEM specific`
  -- , (0xF3, ???)             -- Defined as `OEM specific`
  -- , (0xF4, ???)             -- Defined as `OEM specific`
  -- , (0xF5, ???)             -- Defined as `OEM specific`
  -- , (0xF6, ???)             -- Defined as VK_ATTN
  -- , (0xF7, ???)             -- Defined as VK_CRSEL
  -- , (0xF8, ???)             -- Defined as VK_EXSEL
  -- , (0xF9, ???)             -- Defined as VK_EREOF
  , (0xFA, KeyPlay)
  -- , (0xFB, ???)             -- Defined as VK_ZOOM
  -- , (0xFC, ???)             -- Defined as VK_NONAME
  -- , (0xFD, ???)             -- Defined as VK_PA1
  -- , (0xFE, KeyDelete)       -- Defined as VK_OEM_CLEAR
  ]

-- | Translate a KMonad KeyCode to the corresponding Windows virtual-key code
-- 
-- We cannot simply reverse the above map for the opposite direction, because
-- there will be duplicates where more than one virtual-key code produces the
-- same KMonad KeyCode. See https://github.com/kmonad/kmonad/issues/326
keyCodeToWinCode :: M.HashMap Keycode WinKeycode
keyCodeToWinCode = M.fromList $
  [ -- (KeyReserved, ???)
    (KeyEsc, 0x1B)
  , (Key1, 0x31)
  , (Key2, 0x32)
  , (Key3, 0x33)
  , (Key4, 0x34)
  , (Key5, 0x35)
  , (Key6, 0x36)
  , (Key7, 0x37)
  , (Key8, 0x38)
  , (Key9, 0x39)
  , (Key0, 0x30)
  , (KeyMinus, 0xBD)
  , (KeyEqual, 0xBB)
  , (KeyBackspace, 0x08)
  , (KeyTab, 0x09)
  , (KeyQ, 0x51)
  , (KeyW, 0x57)
  , (KeyE, 0x45)
  , (KeyR, 0x52)
  , (KeyT, 0x54)
  , (KeyY, 0x59)
  , (KeyU, 0x55)
  , (KeyI, 0x49)
  , (KeyO, 0x4F)
  , (KeyP, 0x50)
  , (KeyLeftBrace, 0xDB)
  , (KeyRightBrace, 0xDD)
  , (KeyEnter, 0x0D)
  , (KeyLeftCtrl, 0xA2)
  , (KeyA, 0x41)
  , (KeyS, 0x53)
  , (KeyD, 0x44)
  , (KeyF, 0x46)
  , (KeyG, 0x47)
  , (KeyH, 0x48)
  , (KeyJ, 0x4A)
  , (KeyK, 0x4B)
  , (KeyL, 0x4C)
  , (KeySemicolon, 0xBA)
  , (KeyApostrophe, 0xDE)
  , (KeyGrave, 0xC0)
  , (KeyLeftShift, 0xA0)
  , (KeyBackslash, 0xDC)
  , (KeyZ, 0x5A)
  , (KeyX, 0x58)
  , (KeyC, 0x43)
  , (KeyV, 0x56)
  , (KeyB, 0x42)
  , (KeyN, 0x4E)
  , (KeyM, 0x4D)
  , (KeyComma, 0xBC)
  , (KeyDot, 0xBE)
  , (KeySlash, 0xBF)
  , (KeyRightShift, 0xA1)
  , (KeyKpAsterisk, 0x6A)
  , (KeyLeftAlt, 0xA4)
  , (KeySpace, 0x20)
  , (KeyCapsLock, 0x14)
  , (KeyF1, 0x70)
  , (KeyF2, 0x71)
  , (KeyF3, 0x72)
  , (KeyF4, 0x73)
  , (KeyF5, 0x74)
  , (KeyF6, 0x75)
  , (KeyF7, 0x76)
  , (KeyF8, 0x77)
  , (KeyF9, 0x78)
  , (KeyF10, 0x79)
  , (KeyNumLock, 0x90)
  , (KeyScrollLock, 0x91)
  , (KeyKp7, 0x67)
  , (KeyKp8, 0x68)
  , (KeyKp9, 0x69)
  , (KeyKpMinus, 0x6D)
  , (KeyKp4, 0x64)
  , (KeyKp5, 0x65)
  , (KeyKp6, 0x66)
  , (KeyKpPlus, 0x6B)
  , (KeyKp1, 0x61)
  , (KeyKp2, 0x62)
  , (KeyKp3, 0x63)
  , (KeyKp0, 0x60)
  , (KeyKpDot, 0x6E)
  -- , (Missing84, ???)
  -- , (KeyZenkakuHankaku, ???)
  , (Key102nd, 0xE2)
  , (KeyF11, 0x7A)
  , (KeyF12, 0x7B)
  -- , (KeyRo, ???)
  , (KeyKatakana, 0x15)
  -- , (KeyHiragana, ???)
  , (KeyHenkan, 0x1C)
  , (KeyKatakanaHiragana, 0x15)
  , (KeyMuhenkan, 0x1D)
  -- , (KeyKpjpcomma, ???)
  , (KeyKpEnter, 0x0D)
  , (KeyRightCtrl, 0xA3)
  , (KeyKpSlash, 0x6F)
  -- , (KeySysRq, ???)
  , (KeyRightAlt, 0xA5)
  -- , (KeyLinefeed, ???)
  , (KeyHome, 0x24)
  , (KeyUp, 0x26)
  , (KeyPageUp, 0x21)
  , (KeyLeft, 0x25)
  , (KeyRight, 0x27)
  , (KeyEnd, 0x23)
  , (KeyDown, 0x28)
  , (KeyPageDown, 0x22)
  , (KeyInsert, 0x2D)
  , (KeyDelete, 0x2E)
  -- , (KeyMacro, ???)
  , (KeyMute, 0xAD)
  , (KeyVolumeDown, 0xAE)
  , (KeyVolumeUp, 0xAF)
  -- , (KeyPower, ???)
  -- , (KeyKpEqual, ???)
  -- , (KeyKpPlusMinus, ???)
  , (KeyPause, 0x13)
  -- , (KeyScale, ???)
  -- , (KeyKpComma, ???)
  , (KeyHangeul, 0x15)
  , (KeyHanja, 0x19)
  -- , (KeyYen, ???)
  , (KeyLeftMeta, 0x5B)
  , (KeyRightMeta, 0x5C)
  , (KeyCompose, 0x5D)
  , (KeyStop, 0xA9)
  -- , (KeyAgain, ???)
  -- , (KeyProps, ???)
  -- , (KeyUndo, ???)
  -- , (KeyFront, ???)
  -- , (KeyCopy, ???)
  -- , (KeyOpen, ???)
  -- , (KeyPaste, ???)
  -- , (KeyFind, ???)
  -- , (KeyCut, ???)
  , (KeyHelp, 0x2F)
  , (KeyMenu, 0x5D)
  -- , (KeyCalc, ???)
  -- , (KeySetup, ???)
  , (KeySleep, 0x5F)
  -- , (KeyWakeUp, ???)
  -- , (KeyFile, ???)
  -- , (KeySendFile, ???)
  -- , (KeyDeleteFile, ???)
  -- , (KeyXfer, ???)
  -- , (KeyProg1, ???)
  -- , (KeyProg2, ???)
  -- , (KeyWww, ???)
  -- , (KeyMsDos, ???)
  -- , (KeyCoffee, ???)
  -- , (KeyDirection, ???)
  -- , (KeyCycleWindows, ???)
  , (KeyMail, 0xB4)
  -- , (KeyBookmarks, ???)
  -- , (KeyComputer, ???)
  , (KeyBack, 0xA6)
  , (KeyForward, 0xA7)
  -- , (KeyCloseCd, ???)
  -- , (KeyEjectCd, ???)
  -- , (KeyEjectCloseCd, ???)
  , (KeyNextSong, 0xB0)
  , (KeyPlayPause, 0xB3)
  , (KeyPreviousSong, 0xB1)
  , (KeyStopCd, 0xB2)
  -- , (KeyRecord, ???)
  -- , (KeyRewind, ???)
  -- , (KeyPhone, ???)
  -- , (KeyIso, ???)
  -- , (KeyConfig, ???)
  , (KeyHomepage, 0xAC)
  , (KeyRefresh, 0xA8)
  -- , (KeyExit, ???)
  -- , (KeyMove, ???)
  -- , (KeyEdit, ???)
  -- , (KeyScrollUp, ???)
  -- , (KeyScrollDown, ???)
  -- , (KeyKpLeftParen, ???)
  -- , (KeyKpRightParen, ???)
  -- , (KeyNew, ???)
  -- , (KeyRedo, ???)
  , (KeyF13, 0x7C)
  , (KeyF14, 0x7D)
  , (KeyF15, 0x7E)
  , (KeyF16, 0x7F)
  , (KeyF17, 0x80)
  , (KeyF18, 0x81)
  , (KeyF19, 0x82)
  , (KeyF20, 0x83)
  , (KeyF21, 0x84)
  , (KeyF22, 0x85)
  , (KeyF23, 0x86)
  , (KeyF24, 0x87)
  -- , (Missing195, ???)
  -- , (Missing196, ???)
  -- , (Missing197, ???)
  -- , (Missing198, ???)
  -- , (Missing199, ???)
  -- , (KeyPlayCd, ???)
  -- , (KeyPauseCd, ???)
  -- , (KeyProg3, ???)
  -- , (KeyProg4, ???)
  -- , (KeyDashboard, ???)
  -- , (KeySuspend, ???)
  -- , (KeyClose, ???)
  , (KeyPlay, 0xFA)
  --, (KeyFastForward, ???)
  --, (KeyBassBoost, ???)
  , (KeyPrint, 0x2A)
  -- , (KeyHp, ???)
  -- , (KeyCamera, ???)
  -- , (KeySound, ???)
  -- , (KeyQuestion, ???)
  , (KeyEmail, 0xB4)
  -- , (KeyChat, ???)
  , (KeySearch, 0xAA)
  -- , (KeyConnect, ???)
  -- , (KeyFinance, ???)
  -- , (KeySport, ???)
  -- , (KeyShop, ???)
  -- , (KeyAlterase, ???)
  , (KeyCancel, 0x03)
  -- , (KeyBrightnessDown, ???)
  -- , (KeyBrightnessUp, ???)
  , (KeyMedia, 0xB5)
  -- , (KeySwitchVideoMode, ???)
  -- , (KeyKbdIllumToggle, ???)
  -- , (KeyKbdIllumDown, ???)
  -- , (KeyKbdIllumUp, ???)
  -- , (KeySend, ???)
  -- , (KeyReply, ???)
  -- , (KeyForwardMail, ???)
  -- , (KeySave, ???)
  -- , (KeyDocuments, ???)
  -- , (KeyBattery, ???)
  -- , (KeyBluetooth, ???)
  -- , (KeyWlan, ???)
  -- , (KeyUwb, ???)
  -- , (KeyUnknown, ???)
  , (KeyVideoNext, 0xB0)
  , (KeyVideoPrev, 0xB1)
  -- , (KeyBrightnessCycle, ???)
  -- , (KeyBrightnessZero, ???)
  -- , (KeyDisplayOff, ???)
  -- , (KeyWimax, ???)
  -- , (Missing247, ???)
  -- , (Missing248, ???)
  -- , (Missing249, ???)
  -- , (Missing250, ???)
  -- , (Missing251, ???)
  -- , (Missing252, ???)
  -- , (Missing253, ???)
  -- , (Missing254, ???)
  -- , (Missing255, ???)
  ]
  