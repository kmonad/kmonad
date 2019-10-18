{-# LANGUAGE DeriveAnyClass #-}
{-|
Module      : KMonad.Core.KeyCode
Description : A collection of all KeyCode's in KMonad
Copyright   : (c) David Janssen, 2019
License     : MIT

Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : non-portable (MPTC with FD, FFI to Linux-only c-code)

This module is basically just a Haskell 'Enum' version of the keycodes in
Linux's headers: https://github.com/torvalds/linux/blob/master/include/uapi/linux/input-event-codes.h.
The CAPS_WITH_UNDERSCORES has been converted to CamelCase.

There are some regions missing numbers in the input.h file, and they have been
simply called MissingXX, where XX is the number.

Calling 'toEnum' on a keycode should produce the correctly matching 'KeyCode'
constructor and vice-versa.

-}

module KMonad.Core.KeyCode
  ( -- * The KeyCode type
    KeyCode(..)

    -- * A ClassyLens style class for "Having a KeyCode"
  , HasKeyCode(..)

    -- * A converter from Char to keycode
  , kcFromChar
  )
where

import Control.Lens
import Data.Char
import Data.Hashable (Hashable)
import GHC.Generics (Generic)

import qualified Data.IntMap.Strict as M

-- | The 'KeyCode' type and its constructors. Essentially a gigantic 'Enum'
data KeyCode
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
  deriving stock    (Eq, Show, Bounded, Enum, Ord, Generic)
  deriving anyclass (Hashable)

-- | The 'HasKeyCode' class can be used to indicate that some object contains a
-- 'KeyCode'. This is essentially only used for 'KMonad.Core.Event.KeyEvent's at
-- the moment.
class HasKeyCode a where
  keyCode :: Lens' a KeyCode
instance HasKeyCode KeyCode where
  keyCode = id


-- | There is no easy correspondence between characters and keycodes, but it is
-- sometimes nice to be able to refer to keycodes by their letter or number.
-- This function is just a lookup-table. Both upper- and lower-case chars are
-- mapped to their corresponding keycode. Mappings only exist for letters and
-- numbers.
kcFromChar :: Char -> Maybe KeyCode
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
