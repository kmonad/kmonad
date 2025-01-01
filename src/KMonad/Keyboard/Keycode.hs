{-# LANGUAGE DeriveAnyClass #-}
{-|
Module      : KMonad.Keyboard.Keycode
Description : Description of all possible keycodes.
Copyright   : (c) David Janssen, 2019
License     : MIT
Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : portable

'Keycode's are represented as a large enum lining up the keycodes defined in the Linux headers.

-}
module KMonad.Keyboard.Keycode
  ( -- * The core Keycode type
    -- $typ
    Keycode(..)

    -- * Naming utilities to refer to Keycodes
    -- $names
  , keyNames

  )
where

import KMonad.Prelude

import qualified KMonad.Util.MultiMap     as Q
import qualified RIO.HashSet       as S
import qualified RIO.Text          as T
import qualified RIO.Text.Partial  as T (head)

--------------------------------------------------------------------------------
-- $typ
--
-- 'Keycode's are defined as a large ADT that mimics the keycodes from the Linux
-- headers:
-- https://github.com/torvalds/linux/blob/master/include/uapi/linux/input-event-codes.h.
--
-- Anywhere there are missing regions in the Linux headers, we've defined
-- @MissingXX@ values for the ADT.
--
-- Calling 'RIO.Partial.toEnum' on a Linux keycode value should produce the
-- corresponding 'Keycode' value and vice-versa.

-- | The 'Keycode' datatype, as an 'Enum' of all the values a 'Keycode' can take.
data Keycode
  = KeyReserved
  | KeyEsc
  | Key1
  | Key2
  | Key3
  | Key4
  | Key5
  | Key6
  | Key7
  | Key8
  | Key9
  | Key0
  | KeyMinus
  | KeyEqual
  | KeyBackspace
  | KeyTab
  | KeyQ
  | KeyW
  | KeyE
  | KeyR
  | KeyT
  | KeyY
  | KeyU
  | KeyI
  | KeyO
  | KeyP
  | KeyLeftBrace
  | KeyRightBrace
  | KeyEnter
  | KeyLeftCtrl
  | KeyA
  | KeyS
  | KeyD
  | KeyF
  | KeyG
  | KeyH
  | KeyJ
  | KeyK
  | KeyL
  | KeySemicolon
  | KeyApostrophe
  | KeyGrave
  | KeyLeftShift
  | KeyBackslash
  | KeyZ
  | KeyX
  | KeyC
  | KeyV
  | KeyB
  | KeyN
  | KeyM
  | KeyComma
  | KeyDot
  | KeySlash
  | KeyRightShift
  | KeyKpAsterisk
  | KeyLeftAlt
  | KeySpace
  | KeyCapsLock
  | KeyF1
  | KeyF2
  | KeyF3
  | KeyF4
  | KeyF5
  | KeyF6
  | KeyF7
  | KeyF8
  | KeyF9
  | KeyF10
  | KeyNumLock
  | KeyScrollLock
  | KeyKp7
  | KeyKp8
  | KeyKp9
  | KeyKpMinus
  | KeyKp4
  | KeyKp5
  | KeyKp6
  | KeyKpPlus
  | KeyKp1
  | KeyKp2
  | KeyKp3
  | KeyKp0
  | KeyKpDot
  | Missing84
  | KeyZenkakuHankaku
  | Key102nd
  | KeyF11
  | KeyF12
  | KeyRo
  | KeyKatakana
  | KeyHiragana
  | KeyHenkan
  | KeyKatakanaHiragana
  | KeyMuhenkan
  | KeyKpjpcomma
  | KeyKpEnter
  | KeyRightCtrl
  | KeyKpSlash
  | KeySysRq
  | KeyRightAlt
  | KeyLinefeed
  | KeyHome
  | KeyUp
  | KeyPageUp
  | KeyLeft
  | KeyRight
  | KeyEnd
  | KeyDown
  | KeyPageDown
  | KeyInsert
  | KeyDelete
  | KeyMacro
  | KeyMute
  | KeyVolumeDown
  | KeyVolumeUp
  | KeyPower
  | KeyKpEqual
  | KeyKpPlusMinus
  | KeyPause
  | KeyScale
  | KeyKpComma
  | KeyHangeul
  | KeyHanja
  | KeyYen
  | KeyLeftMeta
  | KeyRightMeta
  | KeyCompose
  | KeyStop
  | KeyAgain
  | KeyProps
  | KeyUndo
  | KeyFront
  | KeyCopy
  | KeyOpen
  | KeyPaste
  | KeyFind
  | KeyCut
  | KeyHelp
  | KeyMenu
  | KeyCalc
  | KeySetup
  | KeySleep
  | KeyWakeUp
  | KeyFile
  | KeySendFile
  | KeyDeleteFile
  | KeyXfer
  | KeyProg1
  | KeyProg2
  | KeyWww
  | KeyMsDos
  | KeyCoffee
  | KeyDirection
  | KeyCycleWindows
  | KeyMail
  | KeyBookmarks
  | KeyComputer
  | KeyBack
  | KeyForward
  | KeyCloseCd
  | KeyEjectCd
  | KeyEjectCloseCd
  | KeyNextSong
  | KeyPlayPause
  | KeyPreviousSong
  | KeyStopCd
  | KeyRecord
  | KeyRewind
  | KeyPhone
  | KeyIso
  | KeyConfig
  | KeyHomepage
  | KeyRefresh
  | KeyExit
  | KeyMove
  | KeyEdit
  | KeyScrollUp
  | KeyScrollDown
  | KeyKpLeftParen
  | KeyKpRightParen
  | KeyNew
  | KeyRedo
  | KeyF13
  | KeyF14
  | KeyF15
  | KeyF16
  | KeyF17
  | KeyF18
  | KeyF19
  | KeyF20
  | KeyF21
  | KeyF22
  | KeyF23
  | KeyF24
  | Missing195
  | Missing196
  | Missing197
  | Missing198
  | Missing199
  | KeyPlayCd
  | KeyPauseCd
  | KeyProg3
  | KeyProg4
  | KeyDashboard
  | KeySuspend
  | KeyClose
  | KeyPlay
  | KeyFastForward
  | KeyBassBoost
  | KeyPrint
  | KeyHp
  | KeyCamera
  | KeySound
  | KeyQuestion
  | KeyEmail
  | KeyChat
  | KeySearch
  | KeyConnect
  | KeyFinance
  | KeySport
  | KeyShop
  | KeyAlterase
  | KeyCancel
  | KeyBrightnessDown
  | KeyBrightnessUp
  | KeyMedia
  | KeySwitchVideoMode
  | KeyKbdIllumToggle
  | KeyKbdIllumDown
  | KeyKbdIllumUp
  | KeySend
  | KeyReply
  | KeyForwardMail
  | KeySave
  | KeyDocuments
  | KeyBattery
  | KeyBluetooth
  | KeyWlan
  | KeyUwb
  | KeyUnknown
  | KeyVideoNext
  | KeyVideoPrev
  | KeyBrightnessCycle
  | KeyBrightnessZero
  | KeyDisplayOff
  | KeyWimax
  | KeyRfkill
  | KeyMicmute
  | Missing249
  | Missing250
  | Missing251
  | Missing252
  | Missing253
  | Missing254
  | Missing255
  | Btn0
  | Btn1
  | Btn2
  | Btn3
  | Btn4
  | Btn5
  | Btn6
  | Btn7
  | Btn8
  | Btn9
  | Missing0x10a
  | Missing0x10b
  | Missing0x10c
  | Missing0x10d
  | Missing0x10e
  | Missing0x10f
  | BtnLeft
  | BtnRight
  | BtnMiddle
  | BtnSide
  | BtnExtra
  | BtnForward
  | BtnBack
  | BtnTask
  | Missing0x118
  | Missing0x119
  | Missing0x11a
  | Missing0x11b
  | Missing0x11c
  | Missing0x11d
  | Missing0x11e
  | Missing0x11f
  | BtnJoystick
  | BtnThumb
  | BtnThumb2
  | BtnTop
  | BtnTop2
  | BtnPinkie
  | BtnBase
  | BtnBase2
  | BtnBase3
  | BtnBase4
  | BtnBase5
  | BtnBase6
  | Missing0x12c
  | Missing0x12d
  | Missing0x12e
  | BtnDead
  | BtnGamepad
  | BtnB
  | BtnC
  | BtnX
  | BtnY
  | BtnZ
  | BtnTL
  | BtnTR
  | BtnTL2
  | BtnTR2
  | BtnSelect
  | BtnStart
  | BtnMode
  | BtnThumbL
  | BtnThumbR
  | Missing0x13f
  | BtnDigi
  | BtnToolRubber
  | BtnToolBrush
  | BtnToolPencil
  | BtnToolAirbrush
  | BtnToolFinger
  | BtnToolMouse
  | BtnToolLens
  | BtnToolQuinttap
  | BtnStylus3
  | BtnTouch
  | BtnStylus
  | BtnStylus2
  | BtnToolDoubletap
  | BtnToolTripletap
  | BtnToolQuadtap
  | BtnGearDown
  | BtnGearUp
  | Missing0x152
  | Missing0x153
  | Missing0x154
  | Missing0x155
  | Missing0x156
  | Missing0x157
  | Missing0x158
  | Missing0x159
  | Missing0x15a
  | Missing0x15b
  | Missing0x15c
  | Missing0x15d
  | Missing0x15e
  | Missing0x15f
  | KeyOk
  | KeySelect
  | KeyGoTo
  | KeyClear
  | KeyPower2
  | KeyOption
  | KeyInfo
  | KeyTime
  | KeyVendor
  | KeyArchive
  | KeyProgram
  | KeyChannel
  | KeyFavorites
  | KeyEpg
  | KeyPvr
  | KeyMhp
  | KeyLanguage
  | KeyTitle
  | KeySubtitle
  | KeyAngle
  | KeyZoom
  | KeyMode
  | KeyKeyboard
  | KeyScreen
  | KeyPc
  | KeyTv
  | KeyTv2
  | KeyVcr
  | KeyVcr2
  | KeySat
  | KeySat2
  | KeyCd
  | KeyTape
  | KeyRadio
  | KeyTuner
  | KeyPlayer
  | KeyText
  | KeyDvd
  | KeyAux
  | KeyMp3
  | KeyAudio
  | KeyVideo
  | KeyDirectory
  | KeyList
  | KeyMemo
  | KeyCalendar
  | KeyRed
  | KeyGreen
  | KeyYellow
  | KeyBlue
  | KeyChannelUp
  | KeyChannelDown
  | KeyFirst
  | KeyLast
  | KeyAb
  | KeyNextTask -- Should be `KeyNext` but conflicts with alias for `KeyNextSong`. So we take the HID Name
  | KeyRestart
  | KeySlow
  | KeyShuffle
  | KeyBreak
  | KeyPreviousTask -- See `KeyNextTask`
  | KeyDigits
  | KeyTeen
  | KeyTwen
  | KeyVideoPhone
  | KeyGames
  | KeyZoomIn
  | KeyZoomOut
  | KeyZoomReset
  | KeyWordProcessor
  | KeyEditor
  | KeySpreadSheet
  | KeyGraphicsEditor
  | KeyPresentation
  | KeyDatabase
  | KeyNews
  | KeyVoicemail
  | KeyAddressBook
  | KeyMessenger
  | KeyBrightnessToggle
  | KeySpellCheck
  | KeyLogoff
  | KeyDollar
  | KeyEuro
  | KeyFrameBack
  | KeyFrameForward
  | KeyContextMenu
  | KeyMediaRepeat
  | Key10channelsUp
  | Key10channelsDown
  | KeyImages
  | Missing0x1bb
  | KeyNotificationCenter
  | KeyPickupPhone
  | KeyHangupPhone
  | Missing0x1bf
  | KeyDelEol
  | KeyDelEos
  | KeyInsLine
  | KeyDelLine
  | Missing0x1c4
  | Missing0x1c5
  | Missing0x1c6
  | Missing0x1c7
  | Missing0x1c8
  | Missing0x1c9
  | Missing0x1ca
  | Missing0x1cb
  | Missing0x1cc
  | Missing0x1cd
  | Missing0x1ce
  | Missing0x1cf
  | KeyFn
  | KeyFnEsc
  | KeyFnF1
  | KeyFnF2
  | KeyFnF3
  | KeyFnF4
  | KeyFnF5
  | KeyFnF6
  | KeyFnF7
  | KeyFnF8
  | KeyFnF9
  | KeyFnF10
  | KeyFnF11
  | KeyFnF12
  | KeyFn1
  | KeyFn2
  | KeyFnD
  | KeyFnE
  | KeyFnF
  | KeyFnS
  | KeyFnB
  | KeyFnRightShift
  | Missing0x1e6
  | Missing0x1e7
  | Missing0x1e8
  | Missing0x1e9
  | Missing0x1ea
  | Missing0x1eb
  | Missing0x1ec
  | Missing0x1ed
  | Missing0x1ee
  | Missing0x1ef
  | Missing0x1f0
  | KeyBrlDot1
  | KeyBrlDot2
  | KeyBrlDot3
  | KeyBrlDot4
  | KeyBrlDot5
  | KeyBrlDot6
  | KeyBrlDot7
  | KeyBrlDot8
  | KeyBrlDot9
  | KeyBrlDot10
  | Missing0x1fb
  | Missing0x1fc
  | Missing0x1fd
  | Missing0x1fe
  | Missing0x1ff
  | KeyNumeric0
  | KeyNumeric1
  | KeyNumeric2
  | KeyNumeric3
  | KeyNumeric4
  | KeyNumeric5
  | KeyNumeric6
  | KeyNumeric7
  | KeyNumeric8
  | KeyNumeric9
  | KeyNumericStar
  | KeyNumericPound
  | KeyNumericA
  | KeyNumericB
  | KeyNumericC
  | KeyNumericD
  | KeyCameraFocus
  | KeyWpsButton
  | KeyTouchpadToggle
  | KeyTouchpadOn
  | KeyTouchpadOff
  | KeyCameraZoomIn
  | KeyCameraZoomOut
  | KeyCameraUp
  | KeyCameraDown
  | KeyCameraLeft
  | KeyCameraRight
  | KeyAttendantOn
  | KeyAttendantOff
  | KeyAttendantToggle
  | KeyLightsToggle
  | Missing0x21f
  | BtnDpadUp
  | BtnDpadDown
  | BtnDpadLeft
  | BtnDpadRight
  | Missing0x224
  | Missing0x225
  | Missing0x226
  | Missing0x227
  | Missing0x228
  | Missing0x229
  | Missing0x22a
  | Missing0x22b
  | Missing0x22c
  | Missing0x22d
  | Missing0x22e
  | Missing0x22f
  | KeyAlsToggle
  | KeyRotateLockToggle
  | KeyRefreshRateToggle
  | Missing0x233
  | Missing0x234
  | Missing0x235
  | Missing0x236
  | Missing0x237
  | Missing0x238
  | Missing0x239
  | Missing0x23a
  | Missing0x23b
  | Missing0x23c
  | Missing0x23d
  | Missing0x23e
  | Missing0x23f
  | KeyButtonconfig
  | KeyTaskmanager
  | KeyJournal
  | KeyControlPanel
  | KeyAppSelect
  | KeyScreenSaver
  | KeyVoicecommand
  | KeyAssistant
  | KeyKbdLayoutNext
  | KeyEmojiPicker
  | KeyDictate
  | KeyCameraAccessEnable
  | KeyCameraAccessDisable
  | KeyCameraAccessToggle
  | KeyAccessibility
  | KeyDoNotDisturb
  | KeyBrightnessMin
  | KeyBrightnessMax
  | Missing0x252
  | Missing0x253
  | Missing0x254
  | Missing0x255
  | Missing0x256
  | Missing0x257
  | Missing0x258
  | Missing0x259
  | Missing0x25a
  | Missing0x25b
  | Missing0x25c
  | Missing0x25d
  | Missing0x25e
  | Missing0x25f
  | KeyKbdInputAssistPrev
  | KeyKbdInputAssistNext
  | KeyKbdInputAssistPrevgroup
  | KeyKbdInputAssistNextgroup
  | KeyKbdInputAssistAccept
  | KeyKbdInputAssistCancel
  | KeyRightUp
  | KeyRightDown
  | KeyLeftUp
  | KeyLeftDown
  | KeyRootMenu
  | KeyMediaTopMenu
  | KeyNumeric11
  | KeyNumeric12
  | KeyAudioDesc
  | Key3dMode
  | KeyNextFavorite
  | KeyStopRecord
  | KeyPauseRecord
  | KeyVod
  | KeyUnmute
  | KeyFastreverse
  | KeySlowreverse
  | KeyData
  | KeyOnScreenKeyboard
  | KeyPrivacyScreenToggle
  | KeySelectiveScreenshot
  | KeyNextElement
  | KeyPreviousElement
  | KeyAutopilotEngageToggle
  | KeyMarkWaypoint
  | KeySos
  | KeyNavChart
  | KeyFishingChart
  | KeySingleRangeRadar
  | KeyDualRangeRadar
  | KeyRadarOverlay
  | KeyTraditionalSonar
  | KeyClearvuSonar
  | KeySidevuSonar
  | KeyNavInfo
  | KeyBrightnessMenu
  | Missing0x28a
  | Missing0x28b
  | Missing0x28c
  | Missing0x28d
  | Missing0x28e
  | Missing0x28f
  | KeyMacro1
  | KeyMacro2
  | KeyMacro3
  | KeyMacro4
  | KeyMacro5
  | KeyMacro6
  | KeyMacro7
  | KeyMacro8
  | KeyMacro9
  | KeyMacro10
  | KeyMacro11
  | KeyMacro12
  | KeyMacro13
  | KeyMacro14
  | KeyMacro15
  | KeyMacro16
  | KeyMacro17
  | KeyMacro18
  | KeyMacro19
  | KeyMacro20
  | KeyMacro21
  | KeyMacro22
  | KeyMacro23
  | KeyMacro24
  | KeyMacro25
  | KeyMacro26
  | KeyMacro27
  | KeyMacro28
  | KeyMacro29
  | KeyMacro30
  | Missing0x2ae
  | Missing0x2af
  | KeyMacroRecordStart
  | KeyMacroRecordStop
  | KeyMacroPresetCycle
  | KeyMacroPreset1
  | KeyMacroPreset2
  | KeyMacroPreset3
  | Missing0x2b6
  | Missing0x2b7
  | KeyKbdLcdMenu1
  | KeyKbdLcdMenu2
  | KeyKbdLcdMenu3
  | KeyKbdLcdMenu4
  | KeyKbdLcdMenu5
  | Missing0x2bd
  | Missing0x2be
  | Missing0x2bf
  | BtnTriggerHappy
  | BtnTriggerHappy2
  | BtnTriggerHappy3
  | BtnTriggerHappy4
  | BtnTriggerHappy5
  | BtnTriggerHappy6
  | BtnTriggerHappy7
  | BtnTriggerHappy8
  | BtnTriggerHappy9
  | BtnTriggerHappy10
  | BtnTriggerHappy11
  | BtnTriggerHappy12
  | BtnTriggerHappy13
  | BtnTriggerHappy14
  | BtnTriggerHappy15
  | BtnTriggerHappy16
  | BtnTriggerHappy17
  | BtnTriggerHappy18
  | BtnTriggerHappy19
  | BtnTriggerHappy20
  | BtnTriggerHappy21
  | BtnTriggerHappy22
  | BtnTriggerHappy23
  | BtnTriggerHappy24
  | BtnTriggerHappy25
  | BtnTriggerHappy26
  | BtnTriggerHappy27
  | BtnTriggerHappy28
  | BtnTriggerHappy29
  | BtnTriggerHappy30
  | BtnTriggerHappy31
  | BtnTriggerHappy32
  | BtnTriggerHappy33
  | BtnTriggerHappy34
  | BtnTriggerHappy35
  | BtnTriggerHappy36
  | BtnTriggerHappy37
  | BtnTriggerHappy38
  | BtnTriggerHappy39
  | BtnTriggerHappy40
  | Missing0x2e8
  | Missing0x2e9
  | Missing0x2ea
  | Missing0x2eb
  | Missing0x2ec
  | Missing0x2ed
  | Missing0x2ee
  | Missing0x2ef
  | Missing0x2f0
  | Missing0x2f1
  | Missing0x2f2
  | Missing0x2f3
  | Missing0x2f4
  | Missing0x2f5
  | Missing0x2f6
  | Missing0x2f7
  | Missing0x2f8
  | Missing0x2f9
  | Missing0x2fa
  | Missing0x2fb
  | Missing0x2fc
  | Missing0x2fd
  | Missing0x2fe
  -- Darwin
  | KeyLaunchpad
  | KeyMissionCtrl
  | KeySpotlight
  | KeyDictation
  deriving (Eq, Show, Bounded, Enum, Ord, Generic, Hashable, Typeable, Data)


instance Display Keycode where
  textDisplay c = (\t -> "<" <> t <> ">") . fromMaybe (tshow c)
    $ minimumByOf (_Just . folded) cmpName (keyNames ^. at c)
    where cmpName a b =
            -- Prefer the shortest, and if equal, lowercased version
            case compare (T.length a) (T.length b) of
              EQ -> compare (T.head b) (T.head a)
              o  -> o

--------------------------------------------------------------------------------
-- $sets

-- | The set of all existing 'Keycode'
kcAll :: S.HashSet Keycode
kcAll = S.fromList [minBound .. maxBound]

-- | The set of all 'Keycode' that are not of the MissingXX pattern
kcNotMissing :: S.HashSet Keycode
kcNotMissing = S.fromList $ kcAll ^.. folded . filtered (T.isPrefixOf "Key" . tshow)

--------------------------------------------------------------------------------
-- $names

-- | Helper function to generate easy name maps
nameKC :: Foldable t
  => (Keycode -> Text)
  -> t Keycode
  -> Q.MultiMap Keycode Text
nameKC f = Q.mkMultiMap . map go . toList
  where go k = (k, [f k, T.toLower $ f k])

-- | A collection of 'Keycode' to 'Text' mappings
keyNames :: Q.MultiMap Keycode Text
keyNames = mconcat
  [ nameKC tshow               kcAll
  , nameKC (T.drop 3 . tshow)  kcNotMissing
  , aliases
  ]

-- | A collection of useful aliases to refer to keycode names
aliases :: Q.MultiMap Keycode Text
aliases = Q.mkMultiMap
  [ (KeyEnter,            ["ret", "return", "ent"])
  , (KeyMinus,            ["min", "-"])
  , (KeyEqual,            ["eql", "="])
  , (KeySleep,            ["zzz"])
  , (KeySpace,            ["spc"])
  , (KeyPageUp,           ["pgup"])
  , (KeyPageDown,         ["pgdn"])
  , (KeyInsert,           ["ins"])
  , (KeyDelete,           ["del"])
  , (KeyVolumeUp,         ["volu"])
  , (KeyVolumeDown,       ["voldwn", "vold"])
  , (KeyBrightnessUp,     ["brup", "bru"])
  , (KeyBrightnessDown,   ["brdown", "brdwn", "brdn"])
  , (KeyLeftAlt,          ["lalt", "alt"])
  , (KeyRightAlt,         ["ralt"])
  , (KeyCompose,          ["comp", "cmps", "cmp"])
  , (KeyLeftShift,        ["lshift", "lshft", "lsft", "shft", "sft"])
  , (KeyRightShift,       ["rshift", "rshft", "rsft"])
  , (KeyLeftCtrl,         ["lctrl", "lctl", "ctl"])
  , (KeyRightCtrl,        ["rctrl", "rctl"])
  , (KeyLeftMeta,         ["lmeta", "lmet", "met"])
  , (KeyRightMeta,        ["rmeta", "rmet"])
  , (KeyBackspace,        ["bks", "bspc"])
  , (KeyCapsLock,         ["caps"])
  , (Key102nd,            ["102d", "lsgt", "nubs"])
  , (KeyForward,          ["fwd"])
  , (KeyScrollLock,       ["scrlck", "slck"])
  , (KeyScrollUp,         ["scrup", "sup"])
  , (KeyScrollDown,       ["scrdn", "sdwn", "sdn"])
  , (KeyPrint,            ["prnt"])
  , (KeyWakeUp,           ["wkup"])
  , (KeyLeft,             ["lft"])
  , (KeyRight,            ["rght"])
  , (KeyLeftBrace,        ["lbrc", "["])
  , (KeyRightBrace,       ["rbrc", "]"])
  , (KeySemicolon,        ["scln", ";"])
  , (KeyApostrophe,       ["apos", "'", "apo"])
  , (KeyGrave,            ["grv", "`"])
  , (KeyBackslash,        ["bksl", "\\"]) -- NOTE: "\\" here is a 1char string, the first \ is consumed by Haskell as an escape character
  , (KeyComma,            ["comm", ","])
  , (KeyDot,              ["."])
  , (KeySlash,            ["/"])
  , (KeyNumLock,          ["nlck"])
  , (KeyKpSlash,          ["kp/"])
  , (KeyKpEnter,          ["kprt"])
  , (KeyKpPlus,           ["kp+"])
  , (KeyKpAsterisk,       ["kp*"])
  , (KeyKpMinus,          ["kp-"])
  , (KeyKpDot,            ["kp."])
  , (KeySysRq,            ["ssrq", "sys"])
  , (KeyKbdIllumDown,     ["bldn"])
  , (KeyKbdIllumUp,       ["blup"])
  , (KeyNextSong,         ["next"])
  , (KeyPlayPause,        ["pp"])
  , (KeyPreviousSong,     ["prev"])
  , (KeyMicmute,          ["micm"])
  , (KeyCoffee,           ["lock"])
  -- Darwin
  , (KeyLaunchpad,        ["lp"])
  , (KeyMissionCtrl,      ["mctl"])
  , (KeySpotlight,        ["spot"])
  , (KeyDictation,        ["dict"])
  -- Japanese
  , (KeyZenkakuHankaku,   ["zeh"])
  , (KeyMuhenkan,         ["muh"])
  , (KeyHenkan,           ["hen"])
  , (KeyKatakanaHiragana, ["kah"])
  -- Aliases from `input-event-codes.h`
  , (KeyMute,            ["mininteresting"])
  , (KeyHangeul,         ["hanguel"])
  , (KeyCoffee,          ["screenlock"])
  , (KeyDirection,       ["rotatedisplay"])
  , (KeyDashboard,       ["allapplications"])
  , (KeyBrightnessZero,  ["brightnessauto"])
  , (KeyWimax,           ["wwan"])
  , (Btn0,               ["btnmisc"])
  , (BtnLeft,            ["btnmouse"])
  , (BtnJoystick,        ["btntrigger"])
  , (BtnGamepad,         ["btna", "btnsouth"])
  , (BtnB,               ["btneast"])
  , (BtnX,               ["btnnorth"])
  , (BtnY,               ["btnwest"])
  , (BtnDigi,            ["btntoolpen"])
  , (BtnGearDown,        ["btnwheel"])
  , (KeyZoom,            ["fullscreen"])
  , (KeyScreen,          ["aspectratio"])
  , (KeyBrightnessToggle,["displaytoggle"])
  , (BtnTriggerHappy,    ["btntriggerhappy1"])
  -- HID Usage Names
  , (KeyBackslash,        ["nonuspound"])
  , (KeyCompose,          ["app", "application"])
  , (KeyOpen,             ["exec", "execute"])
  --, (KeyFront,            ["sel", "select"]) -- conflict with KeySelect
  , (KeyRo,               ["i1", "int1", "international1"])
  , (KeyKatakanaHiragana, ["i2", "int2", "international2"])
  , (KeyYen,              ["i3", "int3", "international3"])
  , (KeyHenkan,           ["i4", "int4", "international4"])
  , (KeyMuhenkan,         ["i5", "int5", "international5"])
  , (KeyKpjpcomma,        ["i6", "int6", "international6"])
  , (KeyHangeul,          ["l1", "lang1"])
  , (KeyHanja,            ["l2", "lang2"])
  , (KeyKatakana,         ["l3", "lang3"])
  , (KeyHiragana,         ["l4", "lang4"])
  , (KeyZenkakuHankaku,   ["l5", "lang5"])
  , (KeyExit,             ["quit"])
  , (KeyNextSong,         ["nexttrack"])
  , (KeyPreviousSong,     ["previoustrack"])
  -- , (KeyStopCd,           ["stop"]) -- conflict with KeyStop
  , (KeyEjectCd,          ["eject"])
  , (KeyVolumeUp,         ["volumeincrement"])
  , (KeyVolumeDown,       ["volumedecrement"])
  , (KeyMail,             ["emailreader"])
  , (KeyFinance,          ["checkbook"])
  , (KeyCalc,             ["calculator"])
  , (KeyFile,             ["localmachinebrowser"])
  , (KeyWww,              ["internetbrowser"])
  , (KeyCoffee,           ["termlock"]) -- conflict with KeyScreenSaver: "screensaver"
  , (KeyHelp,             ["helpcenter"])
  , (KeyMedia,            ["imagebrowser"])
  , (KeySound,            ["audiobrowser"])
  , (KeyProps,            ["properties"])
  -- , (KeyHomepage,         ["home"]) -- conflict with KeyHome
  , (KeyForwardMail,      ["forwardmessage"])
  , (KeyProgram,          ["guide"])
  , (KeyMemo,             ["messages"])
  , (KeyTv2,              ["cable"])
  -- , (KeyPvr,              ["home"]) -- conflict with KeyHome
  , (KeySubtitle,         ["caption"])
  , (KeyVcr2,             ["vcr+"])
  , (KeyMediaRepeat,      ["repeat"])
  , (KeyEditor,           ["texteditor"])
  , (KeyNews,             ["newsreader"])
  , (KeyAddressBook,      ["contacts"])
  , (KeyCalendar,         ["schedule"])
  , (KeyMessenger,        ["instantmessaging"])
  , (KeyInfo,             ["featurebrowser", "tipsbrowser"])
  -- , (KeyZoomReset,        ["zoom"]) -- conflict with KeyZoom
  ]
