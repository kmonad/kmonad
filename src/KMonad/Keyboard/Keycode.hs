{-# LANGUAGE DeriveAnyClass #-}
module KMonad.Keyboard.Keycode
  ( -- * The core Keycode type
    -- $typ
    Keycode(..)

    -- * Helpers to construct Keycodes
    -- $help
  , kcFromChar

    -- * Sets of Keycodes for matching
    -- $sets
  , kcLetters
  , kcNumbers
  , kcAlphanum

    -- * Naming utilities to refer to Keycodes
    -- $names
  , keyNames

  )
where

import Prelude

import Data.Char (toLower)

import KMonad.Util

import qualified Data.MultiMap as Q
import qualified RIO.HashMap   as M
import qualified RIO.HashSet   as S
import qualified RIO.Text      as T

--------------------------------------------------------------------------------
-- $typ
--
-- 'Keycode's are defined as a large ADT that mimics the keycodes from the Linux
-- headers:
-- https://github.com/torvalds/linux/blob/master/include/uapi/linux/input-event-codes.h.
--
-- Anywhere there are missing regions in the linux headers, we've defined
-- *MissingXX* values for the ADT.
--
-- Calling 'toEnum' on a Linux keycode value should produce the corresponding
-- 'Keycode' value and vice-versa.

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
  | KeyKpenter
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
  | Missing247
  | Missing248
  | Missing249
  | Missing250
  | Missing251
  | Missing252
  | Missing253
  | Missing254
  | Missing255
  deriving (Eq, Show, Bounded, Enum, Ord, Generic, Hashable)

-- | 'Keycode's have a 'Serialize' instance, and can therefore be turned into
-- bytes and vice-versa
instance Serialize Keycode

instance PrettyPrint Keycode where
  pprint c = (\t -> "<" <> t <> ">") . fromMaybe (tshow c)
    $ minimumByOf (_Just . folded) (comparing T.length) (keyNames ^. at c)

--------------------------------------------------------------------------------
-- $help

-- | There is no easy correspondence between characters and keycodes, but it is
-- sometimes nice to be able to refer to keycodes by their letter or number.
-- This function is just a lookup-table. Both upper- and lower-case chars are
-- mapped to their corresponding keycode. Mappings only exist for letters and
-- numbers.
kcFromChar :: Char -> Maybe Keycode
kcFromChar c = M.lookup (fromEnum . toLower $ c) m
  where m = M.fromList . map (\x -> x & _1 %~ fromEnum) $
          [ ('a', KeyA)
          , ('b', KeyB)
          , ('c', KeyC)
          , ('d', KeyD)
          , ('e', KeyE)
          , ('f', KeyF)
          , ('g', KeyG)
          , ('h', KeyH)
          , ('i', KeyI)
          , ('j', KeyJ)
          , ('k', KeyK)
          , ('l', KeyL)
          , ('m', KeyM)
          , ('n', KeyN)
          , ('o', KeyO)
          , ('p', KeyP)
          , ('q', KeyQ)
          , ('r', KeyR)
          , ('s', KeyS)
          , ('t', KeyT)
          , ('u', KeyU)
          , ('v', KeyV)
          , ('w', KeyW)
          , ('x', KeyX)
          , ('y', KeyY)
          , ('z', KeyZ)
          , ('0', Key0)
          , ('1', Key1)
          , ('2', Key2)
          , ('3', Key3)
          , ('4', Key4)
          , ('5', Key5)
          , ('6', Key6)
          , ('7', Key7)
          , ('8', Key8)
          , ('9', Key9)
          ]

--------------------------------------------------------------------------------
-- $sets

kcAll :: S.HashSet Keycode
kcAll = S.fromList $ [minBound .. maxBound]

kcLetters :: S.HashSet Keycode
kcLetters = S.fromList $ [KeyQ .. KeyP] <> [KeyA .. KeyL] <> [KeyZ .. KeyM]

kcNumbers :: S.HashSet Keycode
kcNumbers = S.fromList $ [Key1 .. Key0]

kcAlphanum :: S.HashSet Keycode
kcAlphanum = kcLetters <> kcNumbers

kcNotMissing :: S.HashSet Keycode
kcNotMissing = S.fromList $ kcAll ^.. folded . filtered (T.isPrefixOf "Key" . tshow)

--------------------------------------------------------------------------------
-- $names

-- | Helper function to generate easy name maps
nameKC :: Foldable t
  => (Keycode -> Name)
  -> t Keycode
  -> Q.MultiMap Keycode Name
nameKC f = Q.mkMultiMap . map go . toList
  where go k = (k, [f k, T.toLower $ f k])

-- | A collection of 'Keycode' to 'Name' mappings
keyNames :: Q.MultiMap Keycode Name
keyNames = mconcat
  [ nameKC tshow               kcAll
  , nameKC (T.drop 3 . tshow)  kcNotMissing
  , aliases
  ]

-- | A collection of useful aliases to refer to keycode names
aliases :: Q.MultiMap Keycode Name
aliases = Q.mkMultiMap
  [ (KeyEnter,          ["ret", "return", "ent"])
  , (KeyMinus,          ["min", "-"])
  , (KeyEqual,          ["eql", "="])
  , (KeySleep,          ["zzz"])
  , (KeySpace,          ["spc"])
  , (KeyPageUp,         ["pgup"])
  , (KeyPageDown,       ["pgdn"])
  , (KeyInsert,         ["ins"])
  , (KeyDelete,         ["del"])
  , (KeyVolumeUp,       ["volu"])
  , (KeyVolumeDown,     ["voldwn", "vold"])
  , (KeyBrightnessUp,   ["brup", "bru"])
  , (KeyBrightnessDown, ["brdown", "brdwn", "brdn"])
  , (KeyLeftAlt,        ["lalt"])
  , (KeyRightAlt,       ["ralt"])
  , (KeyCompose,        ["comp", "cmps"])
  , (KeyLeftShift,      ["lshift", "lshft", "lsft"])
  , (KeyRightShift,     ["rshift", "rshft", "rsft"])
  , (KeyLeftCtrl,       ["lctrl", "lctl"])
  , (KeyRightCtrl,      ["rctrl", "rctl"])
  , (KeyLeftMeta,       ["lmeta", "lmet"])
  , (KeyRightMeta,      ["rmeta", "rmet"])
  , (KeyBackspace,      ["bks", "bspc"])
  , (KeyCapsLock,       ["caps"])
  , (KeyGrave,          ["grv"])
  , (Key102nd,          ["102d"])
  , (KeyForward,        ["fwd"])
  , (KeyScrollLock,     ["scrlck", "slck"])
  , (KeyPrint,          ["prnt"])
  , (KeyWakeUp,         ["wkup"])
  , (KeyLeft,           ["lft"])
  , (KeyRight,          ["rght"])
  , (KeyLeftBrace,      ["lbrc", "["])
  , (KeyRightBrace,     ["rbrc", "]"])
  , (KeySemicolon,      ["scln", ";"])
  , (KeyApostrophe,     ["apos", "'"])
  , (KeyGrave,          ["grv", "`"])
  , (KeyBackslash,      ["bksl", "\\"])
  , (KeyComma,          ["comm", ","])
  , (KeyDot,            ["."])
  ]
