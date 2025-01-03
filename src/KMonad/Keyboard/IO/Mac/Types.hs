module KMonad.Keyboard.IO.Mac.Types
  ( MacError(..)
  , MacKeyEvent
  , toMacKeyEvent
  , fromMacKeyEvent
  , kcMapRaw
  )

where

import KMonad.Prelude

import Foreign.Storable
import KMonad.Keyboard

import qualified RIO.HashMap as M
import RIO.List (sortOn)


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
  -- We sort the reversed list, since some keycodes are duplicates (e.g.: KeyBackslash) and the later takes precedence
  where revMap = M.fromList . sortOn (Down . snd) $ M.toList kcMap ^.. folded . swapped

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

-- | Mac mostly uses the HID names instead. Since Linux doesn't we need to translate.
--
-- For Mac keycodes see https://github.com/apple-opensource/IOHIDFamily/blob/master/IOHIDFamily/IOHIDUsageTables.h
--
-- For HID to Linux mappings:
-- - See the source code where it's defined at https://github.com/torvalds/linux/blob/master/drivers/hid/hid-input.c
-- - See the table provided by the android docs at https://source.android.com/docs/core/interaction/input/keyboard-devices
kcMap :: M.HashMap MacKeycode Keycode
kcMap = M.fromList kcMapRaw

kcMapRaw :: [(MacKeycode, Keycode)]
kcMapRaw =
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
  , ((0x7,0x32), KeyBackslash) -- NonUsPound
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
  , ((0x7,0x46), KeyPrint) -- KeySysRq
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
  , ((0x7,0x64), Key102nd)
  , ((0x7,0x65), KeyCompose)
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
  , ((0x7,0x74), KeyOpen)
  , ((0x7,0x75), KeyHelp)
  , ((0x7,0x76), KeyMenu) -- KeyProps
  , ((0x7,0x77), KeyFront)
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
  , ((0x7,0x87), KeyRo)
  , ((0x7,0x88), KeyKatakanaHiragana)
  , ((0x7,0x89), KeyYen)
  , ((0x7,0x8A), KeyHenkan)
  , ((0x7,0x8B), KeyMuhenkan)
  , ((0x7,0x8C), KeyKpjpcomma)
  -- , ((0x7,0x8D), KeyInternational7)
  -- , ((0x7,0x8E), KeyInternational8)
  -- , ((0x7,0x8F), KeyInternational9)
  , ((0x7,0x90), KeyHangeul)
  , ((0x7,0x91), KeyHanja)
  , ((0x7,0x92), KeyKatakana)
  , ((0x7,0x93), KeyHiragana)
  , ((0x7,0x94), KeyZenkakuHankaku)
  -- , ((0x7,0x95), KeyLANG6)
  -- , ((0x7,0x96), KeyLANG7)
  -- , ((0x7,0x97), KeyLANG8)
  -- , ((0x7,0x98), KeyLANG9)
  -- , ((0x7,0x99), KeyAlternateErase)
  -- , ((0x7,0x9A), KeySysReqOrAttention)
  , ((0x7,0x9B), KeyCancel) -- Does not exists in Linux
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
  , ((0xC,0x34), KeySleep)
  , ((0xC,0x40), KeyMenu)
  , ((0xC,0x45), KeyRight)
  , ((0xC,0x61), KeySubtitle)
  , ((0xC,0x69), KeyRed)
  , ((0xC,0x6A), KeyGreen)
  , ((0xC,0x6B), KeyBlue)
  , ((0xC,0x6C), KeyYellow)
  , ((0xC,0x83), KeyLast)
  -- , ((0xC,0x84), Key???) -- Enter Channel
  -- , ((0xC,0x85), Key???) -- Order Movie
  , ((0xC,0x88), KeyPc)
  , ((0xC,0x89), KeyTv)
  , ((0xC,0x8A), KeyWww)
  , ((0xC,0x8B), KeyDvd)
  , ((0xC,0x8C), KeyPhone)
  , ((0xC,0x8D), KeyProgram)
  , ((0xC,0x8E), KeyVideoPhone)
  , ((0xC,0x8F), KeyGames)
  , ((0xC,0x90), KeyMemo)
  , ((0xC,0x91), KeyCd)
  , ((0xC,0x92), KeyVcr)
  , ((0xC,0x93), KeyTuner)
  , ((0xC,0x94), KeyExit)
  , ((0xC,0x95), KeyHelp)
  , ((0xC,0x96), KeyTape)
  , ((0xC,0x97), KeyTv2)
  , ((0xC,0x98), KeySat)
  -- , ((0xC,0x99), Key???) -- Media Select Security
  , ((0xC,0x9A), KeyPvr)
  -- , ((0xC,0x9B), Key???) -- MediaSelectCall
  , ((0xC,0x9C), KeyChannelUp)
  , ((0xC,0x9D), KeyChannelDown)
  -- , ((0xC,0x9E), Key???) -- Media Select SAP (MacOS: Media)
  , ((0xC,0xA0), KeyVcr2)
  -- , ((0xC,0xA1), Key???) -- Once
  -- , ((0xC,0xA2), Key???) -- Daily
  -- , ((0xC,0xA3), Key???) -- Weekly
  -- , ((0xC,0xA4), Key???) -- Monthly
  , ((0xC,0xB0), KeyPlay)
  , ((0xC,0xB1), KeyPause)
  , ((0xC,0xB2), KeyRecord)
  , ((0xC,0xB3), KeyFastForward)
  , ((0xC,0xB4), KeyRewind)
  , ((0xC,0xB5), KeyNextSong)
  , ((0xC,0xB6), KeyPreviousSong)
  , ((0xC,0xB7), KeyStopCd)
  , ((0xC,0xB8), KeyEjectCd)
  -- , ((0xC,0xB9), Key???) -- Random Play
  -- , ((0xC,0xBA), Key???) -- Select Disc
  -- , ((0xC,0xBB), Key???) -- Enter Disc
  , ((0xC,0xBC), KeyMediaRepeat)
  -- , ((0xC,0xBD), Key???) -- MacOS: Tracking
  -- , ((0xC,0xBE), Key???) -- Track Normal
  -- , ((0xC,0xBF), Key???) -- MacOS: SlowTraciking
  -- , ((0xC,0xC0), Key???) -- Frame Forward
  -- , ((0xC,0xC1), Key???) -- Frame Back
  -- , ((0xC,0xC2), Key???) -- Mark
  -- , ((0xC,0xC3), Key???) -- Clear Mark
  -- , ((0xC,0xC4), Key???) -- Repeat From Mark
  -- , ((0xC,0xC5), Key???) -- Return To Mark
  -- , ((0xC,0xC6), Key???) -- Search Mark Forward
  -- , ((0xC,0xC7), Key???) -- Search Mark Backwards
  -- , ((0xC,0xC8), Key???) -- Counter Reset
  -- , ((0xC,0xC9), Key???) -- Show Counter
  -- , ((0xC,0xCA), Key???) -- Tracking Increment
  -- , ((0xC,0xCB), Key???) -- Tracking Decrement
  -- , ((0xC,0xCC), Key???) -- Stop / Eject
  , ((0xC,0xCD), KeyPlayPause)
  -- , ((0xC,0xCE), Key???) -- Play / Skip
  , ((0xC,0xCF), KeyDictation) -- No HID
  -- , ((0xC,0xE0), Key???) -- MacOS: Volume (linear control)
  -- , ((0xC,0xE1), Key???) -- MacOS: Balance (linear control)
  , ((0xC,0xE2), KeyPlayPause)
  -- , ((0xC,0xE3), Key???) -- MacOS: Bass (linear control)
  -- , ((0xC,0xE4), Key???) -- MacOS: Treble (linear control)
  , ((0xC,0xE5), KeyBassBoost)
  -- , ((0xC,0xE6), Key???) -- Surround Mode
  -- , ((0xC,0xE7), Key???) -- Loudness
  -- , ((0xC,0xE8), Key???) -- MPX
  , ((0xC,0xE9), KeyVolumeUp) -- VolumeIncrement
  , ((0xC,0xEA), KeyVolumeDown) -- VolumentDecrement
  -- , ((0xC,0x100), KeyFanEnable)
  -- , ((0xC,0x101), KeyFanSpeed) -- (linear control)
  -- , ((0xC,0x102), KeyLightEnable)
  -- , ((0xC,0x103), KeyLightIlluminationLevel) -- (linear control)
  -- , ((0xC,0x104), KeyClimateControlEnable)
  -- , ((0xC,0x105), KeyRoomTemperature) -- (linear control)
  -- , ((0xC,0x106), KeySecurityEnable)
  -- , ((0xC,0x107), KeyFireAlarm)
  -- , ((0xC,0x108), KeyPoliceAlarm)
  -- , ((0xC,0x109), KeyProximity) -- (linear control)
  -- , ((0xC,0x10A), KeyMotion)
  -- , ((0xC,0x10B), KeyDuressAlarm)
  -- , ((0xC,0x10C), KeyHoldupAlarm)
  -- , ((0xC,0x10D), KeyMedicalAlarm)
  -- 0x10E - 0x14F Reserved
  -- , ((0xC,0x150), KeyBalanceRight)
  -- , ((0xC,0x151), KeyBalanceLeft)
  -- , ((0xC,0x152), KeyBassIncrement)
  -- , ((0xC,0x153), KeyBassDecrement)
  -- , ((0xC,0x154), KeyTrebleIncrement)
  -- , ((0xC,0x155), KeyTrebleDecrement)
  -- 0x156 - 0x15F Reserved
  -- , ((0xC,0x160), KeySpeakerSystem)
  -- , ((0xC,0x161), KeyChannelLeft)
  -- , ((0xC,0x162), KeyChannelRight)
  -- , ((0xC,0x163), KeyChannelCenter)
  -- , ((0xC,0x164), KeyChannelFront)
  -- , ((0xC,0x165), KeyChannelCenterFront)
  -- , ((0xC,0x166), KeyChannelSide)
  -- , ((0xC,0x167), KeyChannelSurround)
  -- , ((0xC,0x168), KeyChannelLowFrequencyEnhancement)
  -- , ((0xC,0x169), KeyChannelTop)
  -- , ((0xC,0x16A), KeyChannelUnknown)
  -- 0x16B - 0x16F Reserved
  -- , ((0xC,0x170), KeySubChannel) -- (linear control)
  -- , ((0xC,0x171), KeySubChannelIncrement)
  -- , ((0xC,0x172), KeySubChannelDecrement)
  -- , ((0xC,0x173), KeyAlternateAudioIncrement)
  -- , ((0xC,0x174), KeyAlternateAudioDecrement)
  -- 0x175 - 0x17F Reserved
  -- , ((0xC,0x180), KeyApplicationLaunchButtons)
  -- , ((0xC,0x181), KeyALLaunchButtonConfigurationTool)
  , ((0xC,0x182), KeyBookmarks) -- KeyALProgrammableButtonConfiguration
  , ((0xC,0x183), KeyConfig) -- KeyALConsumerControlConfiguration
  , ((0xC,0x184), KeyWordProcessor)
  , ((0xC,0x185), KeyEditor)
  , ((0xC,0x186), KeySpreadSheet)
  , ((0xC,0x187), KeyGraphicsEditor)
  , ((0xC,0x188), KeyPresentation)
  , ((0xC,0x189), KeyDatabase)
  , ((0xC,0x18A), KeyMail)
  , ((0xC,0x18B), KeyNews)
  , ((0xC,0x18C), KeyVoicemail)
  , ((0xC,0x18D), KeyAddressBook)
  , ((0xC,0x18E), KeyCalendar)
  -- , ((0xC,0x18F), KeyALTaskOrProjectManager)
  -- , ((0xC,0x190), KeyALLogOrJournalOrTimecard)
  , ((0xC,0x191), KeyFinance)
  , ((0xC,0x192), KeyCalc)
  -- , ((0xC,0x193), KeyALAOrVCaptureOrPlayback)
  , ((0xC,0x194), KeyFile)
  -- , ((0xC,0x195), KeyALLANOrWANBrowser)
  , ((0xC,0x196), KeyWww)
  -- , ((0xC,0x197), KeyALRemoteNetworkingOrISPConnect)
  -- , ((0xC,0x198), KeyALNetworkConference)
  , ((0xC,0x199), KeyChat)
  -- , ((0xC,0x19A), KeyALTelephonyOrDialer)
  -- , ((0xC,0x19B), KeyALLogon)
  , ((0xC,0x19C), KeyLogoff)
  -- , ((0xC,0x19D), KeyALLogonOrLogoff)
  , ((0xC,0x19E), KeyCoffee)
  -- , ((0xC,0x19F), KeyALControlPanel)
  -- , ((0xC,0x1A0), KeyALCommandLineProcessorOrRun)
  -- , ((0xC,0x1A1), KeyALProcessOrTaskManager)
  -- , ((0xC,0x1A2), KeyAL)
  -- , ((0xC,0x1A3), KeyALNextTaskOrApplication)
  -- , ((0xC,0x1A4), KeyALPreviousTaskOrApplication)
  -- , ((0xC,0x1A5), KeyALPreemptiveHaltTaskOrApplication)
  , ((0xC,0x1A6), KeyHelp) -- KeyALIntegratedHelpCenter
  , ((0xC,0x1A7), KeyDocuments)
  -- , ((0xC,0x1A8), KeyALThesaurus)
  -- , ((0xC,0x1A9), KeyALDictionary)
  -- , ((0xC,0x1AA), KeyALDesktop)
  , ((0xC,0x1AB), KeySpellCheck)
  -- , ((0xC,0x1AC), KeyALGrammerCheck)
  -- , ((0xC,0x1AD), KeyALWirelessStatus)
  -- , ((0xC,0x1AE), KeyALKeyboardLayout)
  -- , ((0xC,0x1AF), KeyALVirusProtection)
  -- , ((0xC,0x1B0), KeyALEncryption)
  -- , ((0xC,0x1B1), KeyALScreenSaver)
  -- , ((0xC,0x1B2), KeyALAlarms)
  -- , ((0xC,0x1B3), KeyALClock)
  -- , ((0xC,0x1B4), KeyALFileBrowser)
  -- , ((0xC,0x1B5), KeyALPowerStatus)
  , ((0xC,0x1B6), KeyMedia)
  , ((0xC,0x1B7), KeySound)
  -- , ((0xC,0x1B8), KeyALMovieBrowser)
  -- , ((0xC,0x1B9), KeyALDigitalRightsManager)
  -- , ((0xC,0x1BA), KeyALDigitalWallet)
  -- 0x1BB Reserved
  , ((0xC,0x1BC), KeyMessenger)
  , ((0xC,0x1BD), KeyInfo)
  -- , ((0xC,0x1BE), KeyALOEMHelp)
  -- , ((0xC,0x1BF), KeyALOnlineCommunity)
  -- , ((0xC,0x1C0), KeyALEntertainmentContentBrowser)
  -- , ((0xC,0x1C1), KeyALOnlineShoppingBrowswer)
  -- , ((0xC,0x1C2), KeyALSmartCardInformationOrHelp)
  -- , ((0xC,0x1C3), KeyALMarketMonitorOrFinanceBrowser)
  -- , ((0xC,0x1C4), KeyALCustomizedCorporateNewsBrowser)
  -- , ((0xC,0x1C5), KeyALOnlineActivityBrowswer)
  -- , ((0xC,0x1C6), KeyALResearchOrSearchBrowswer)
  -- , ((0xC,0x1C7), KeyALAudioPlayer)
  -- , ((0xC,0x1C8), KeyALMessageStatus)
  -- , ((0xC,0x1C9), KeyALContactSync)
  -- , ((0xC,0x1CA), KeyALNavigation)
  -- , ((0xC,0x1CB), KeyALContextawareDesktopAssistant)
  -- 0x1CC - 0x1FF Reserved
  -- , ((0xC,0x200), KeyGenericGUIApplicationControls)
  , ((0xC,0x201), KeyNew)
  , ((0xC,0x202), KeyOpen)
  , ((0xC,0x203), KeyClose)
  , ((0xC,0x204), KeyExit)
  -- , ((0xC,0x205), KeyACMaximize)
  -- , ((0xC,0x206), KeyACMinimize)
  , ((0xC,0x207), KeySave)
  , ((0xC,0x208), KeyPrint)
  , ((0xC,0x209), KeyProps)
  , ((0xC,0x21A), KeyUndo)
  , ((0xC,0x21B), KeyCopy)
  , ((0xC,0x21C), KeyCut)
  , ((0xC,0x21D), KeyPaste)
  -- , ((0xC,0x21E), KeyAC) -- AC Select All
  , ((0xC,0x21F), KeyFind)
  -- , ((0xC,0x220), KeyACFindandReplace)
  , ((0xC,0x221), KeySearch)
  , ((0xC,0x222), KeyGoTo)
  , ((0xC,0x223), KeyHomepage)
  , ((0xC,0x224), KeyBack)
  , ((0xC,0x225), KeyForward)
  , ((0xC,0x226), KeyStop)
  , ((0xC,0x227), KeyRefresh)
  -- , ((0xC,0x228), KeyACPreviousLink)
  -- , ((0xC,0x229), KeyACNextLink)
  , ((0xC,0x22A), KeyBookmarks)
  -- , ((0xC,0x22B), KeyACHistory)
  -- , ((0xC,0x22C), KeyACSubscriptions)
  , ((0xC,0x22D), KeyZoomIn)
  , ((0xC,0x22E), KeyZoomOut)
  , ((0xC,0x22F), KeyZoomReset)
  -- , ((0xC,0x230), KeyACFullScreenView)
  -- , ((0xC,0x231), KeyACNormalView)
  -- , ((0xC,0x232), KeyACViewToggle)
  , ((0xC,0x233), KeyScrollUp)
  , ((0xC,0x234), KeyScrollDown)
  -- , ((0xC,0x235), KeyACScroll)
  -- , ((0xC,0x236), KeyACPanLeft)
  -- , ((0xC,0x237), KeyACPanRight)
  -- , ((0xC,0x238), KeyACPan)
  -- , ((0xC,0x239), KeyACNewWindow)
  -- , ((0xC,0x23A), KeyACTileHorizontally)
  -- , ((0xC,0x23B), KeyACTileVertically)
  -- , ((0xC,0x23C), KeyACFormat)
  -- , ((0xC,0x23D), KeyACEdit)
  -- , ((0xC,0x23E), KeyACBold)
  -- , ((0xC,0x23F), KeyACItalics)
  -- , ((0xC,0x240), KeyACUnderline)
  -- , ((0xC,0x241), KeyACStrikethrough)
  -- , ((0xC,0x242), KeyACSubscript)
  -- , ((0xC,0x243), KeyACSuperscript)
  -- , ((0xC,0x244), KeyACAllCaps)
  -- , ((0xC,0x245), KeyACRotate)
  -- , ((0xC,0x246), KeyACResize)
  -- , ((0xC,0x247), KeyACFlipHorizontal)
  -- , ((0xC,0x248), KeyACFlipVertical)
  -- , ((0xC,0x249), KeyACMirrorHorizontal)
  -- , ((0xC,0x24A), KeyACMirrorVertical)
  -- , ((0xC,0x24B), KeyACFontSelect)
  -- , ((0xC,0x24C), KeyACFontColor)
  -- , ((0xC,0x24D), KeyACFontSize)
  -- , ((0xC,0x24E), KeyACJustifyLeft)
  -- , ((0xC,0x24F), KeyACJustifyCenterH)
  -- , ((0xC,0x250), KeyACJustifyRight)
  -- , ((0xC,0x251), KeyACJustifyBlockH)
  -- , ((0xC,0x252), KeyACJustifyTop)
  -- , ((0xC,0x253), KeyACJustifyCenterV)
  -- , ((0xC,0x254), KeyACJustifyBottom)
  -- , ((0xC,0x255), KeyACJustifyBlockV)
  -- , ((0xC,0x256), KeyACIndentyDecrease)
  -- , ((0xC,0x257), KeyACIndentyIncrease)
  -- , ((0xC,0x258), KeyACNumberedList)
  -- , ((0xC,0x259), KeyACRestartNumbering)
  -- , ((0xC,0x25A), KeyACBulletedList)
  -- , ((0xC,0x25B), KeyACPromote)
  -- , ((0xC,0x25C), KeyACDemote)
  -- , ((0xC,0x25D), KeyACYes)
  -- , ((0xC,0x25E), KeyACNo)
  , ((0xC,0x25F), KeyCancel) -- AC Cancel
  -- , ((0xC,0x260), KeyACCatalog)
  -- , ((0xC,0x261), KeyACBuyOrCheckout)
  -- , ((0xC,0x262), KeyACAddToCart)
  -- , ((0xC,0x263), KeyACExpand)
  -- , ((0xC,0x264), KeyACExpandAll)
  -- , ((0xC,0x265), KeyACCollapse)
  -- , ((0xC,0x266), KeyACCollapseAll)
  -- , ((0xC,0x267), KeyACPrintPreview)
  -- , ((0xC,0x268), KeyACPasteSpecial)
  -- , ((0xC,0x269), KeyACInsertMode)
  -- , ((0xC,0x26A), KeyACDelete)
  -- , ((0xC,0x26B), KeyACLock)
  -- , ((0xC,0x26C), KeyACUnlock)
  -- , ((0xC,0x26D), KeyACProtect)
  -- , ((0xC,0x26E), KeyACUnprotect)
  -- , ((0xC,0x26F), KeyACAttachComment)
  -- , ((0xC,0x270), KeyACDetachComment)
  -- , ((0xC,0x271), KeyACViewComment)
  -- , ((0xC,0x272), KeyACSelectWord)
  -- , ((0xC,0x273), KeyACSelectSentence)
  -- , ((0xC,0x274), KeyACSelectParagraph)
  -- , ((0xC,0x275), KeyACSelectColumn)
  -- , ((0xC,0x276), KeyACSelectRow)
  -- , ((0xC,0x277), KeyACSelectTable)
  -- , ((0xC,0x278), KeyACSelectObject)
  , ((0xC,0x279), KeyRedo)
  -- , ((0xC,0x27A), KeyACSort)
  -- , ((0xC,0x27B), KeyACSortAscending)
  -- , ((0xC,0x27C), KeyACSortDescending)
  -- , ((0xC,0x27D), KeyACFilter)
  -- , ((0xC,0x27E), KeyACSetClock)
  -- , ((0xC,0x27F), KeyACViewClock)
  -- , ((0xC,0x280), KeyACSelectTimeZone)
  -- , ((0xC,0x281), KeyACEditTimeZones)
  -- , ((0xC,0x282), KeyACSetAlarm)
  -- , ((0xC,0x283), KeyACClearAlarm)
  -- , ((0xC,0x284), KeyACSnoozeAlarm)
  -- , ((0xC,0x285), KeyACResetAlarm)
  -- , ((0xC,0x286), KeyACSynchronize)
  -- , ((0xC,0x287), KeyACSendOrReceive)
  -- , ((0xC,0x288), KeyACSendTo)
  , ((0xC,0x289), KeyReply)
  -- , ((0xC,0x28A), KeyACReplyAll)
  , ((0xC,0x28B), KeyForwardMail)
  , ((0xC,0x28C), KeySend)
  -- , ((0xC,0x28D), KeyACAttachFile)
  -- , ((0xC,0x28E), KeyACUpload)
  -- , ((0xC,0x28F), KeyACDownload)
  -- , ((0xC,0x290), KeyACSetBorders)
  -- , ((0xC,0x291), KeyACInsertRow)
  -- , ((0xC,0x292), KeyACInsertColumn)
  -- , ((0xC,0x293), KeyACInsertFile)
  -- , ((0xC,0x294), KeyACInsertPicture)
  -- , ((0xC,0x295), KeyACInsertObject)
  -- , ((0xC,0x296), KeyACInsertSymbol)
  -- , ((0xC,0x297), KeyACSaveAndClose)
  -- , ((0xC,0x298), KeyACRename)
  -- , ((0xC,0x299), KeyACMerge)
  -- , ((0xC,0x29A), KeyACSplit)
  -- , ((0xC,0x29B), KeyACDistributeH)
  -- , ((0xC,0x29C), KeyACDistributeV)
  -- , ((0xC,0x29D), KeyACKeyboardLayoutSelect)
  -- , ((0xC,0x29E), KeyACNavigationGuidance)
  -- , ((0xC,0x29F), KeyACDesktopShowAllWindows)
  -- , ((0xC,0x2A0), KeyACSoftKeyLeft)
  -- , ((0xC,0x2A1), KeyACSoftKeyRight)
  -- , ((0xC,0x2A2), KeyACDesktopShowAllApplications)
  -- 0x02A3 - 0x02AF Reserved
  -- , ((0xC,0x2B0), KeyACIdleKeepAlive)
  -- 0x02B1 - 0x02BF Reserved
  -- , ((0xC,0x2C0), KeyExtendedKeyboardAttributesCollection)
  -- , ((0xC,0x2C1), KeyKeyboardFormFactor)
  -- , ((0xC,0x2C2), KeyKeyboardKeyType)
  -- , ((0xC,0x2C3), KeyKeyboardPhysicalLayout)
  -- , ((0xC,0x2C4), KeyVendorSpecificKeyboardPhysicalLayout)
  -- , ((0xC,0x2C5), KeyKeyboardIETFLanguageTagIndex)
  -- , ((0xC,0x2C6), KeyImplementedKeyboardInputAssistControls)
  -- , ((0xC,0x2C7), KeyKeyboardInputAssistPrevious)
  -- , ((0xC,0x2C8), KeyKeyboardInputAssistNext)
  -- , ((0xC,0x2C9), KeyKeyboardInputAssistPreviousGroup)
  -- , ((0xC,0x2CA), KeyKeyboardInputAssistNextGroup)
  -- , ((0xC,0x2CB), KeyKeyboardInputAssistAccept)
  -- , ((0xC,0x2CC), KeyKeyboardInputAssistCancel)
  -- 0x02CD - 0x04FF Reserved
  -- , ((0xC,0x500), KeyContactEdited)
  -- , ((0xC,0x501), KeyContactAdded)
  -- , ((0xC,0x502), KeyContactRecordActive)
  -- , ((0xC,0x503), KeyContactIndex)
  -- , ((0xC,0x504), KeyContactNickname)
  -- , ((0xC,0x505), KeyContactFirstName)
  -- , ((0xC,0x506), KeyContactLastName)
  -- , ((0xC,0x507), KeyContactFullName)
  -- , ((0xC,0x508), KeyContactPhoneNumberPersonal)
  -- , ((0xC,0x509), KeyContactPhoneNumberBusiness)
  -- , ((0xC,0x50A), KeyContactPhoneNumberMobile)
  -- , ((0xC,0x50B), KeyContactPhoneNumberPager)
  -- , ((0xC,0x50C), KeyContactPhoneNumberFax)
  -- , ((0xC,0x50D), KeyContactPhoneNumberOther)
  -- , ((0xC,0x50E), KeyContactEmailPersonal)
  -- , ((0xC,0x50F), KeyContactEmailBusiness)
  -- , ((0xC,0x510), KeyContactEmailOther)
  -- , ((0xC,0x511), KeyContactEmailMain)
  -- , ((0xC,0x512), KeyContactSpeedDialNumber)
  -- , ((0xC,0x513), KeyContactStatusFlag)
  -- , ((0xC,0x514), KeyContactMisc)
  , ((0xFF,0x3), KeyFn)
  , ((0xFF,0x4), KeyBrightnessUp)
  , ((0xFF,0x5), KeyBrightnessDown)
  , ((0xFF,0x8), KeyKbdIllumUp)
  , ((0xFF,0x9), KeyKbdIllumDown)
  , ((0xFF01,0x1), KeySpotlight)
  , ((0xFF01,0x4), KeyLaunchpad)
  , ((0xFF01,0x10), KeyMissionCtrl)
  ]
