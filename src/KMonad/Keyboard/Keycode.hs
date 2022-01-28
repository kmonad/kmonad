{-# LANGUAGE DeriveAnyClass, CPP #-}
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
-- Anywhere there are missing regions in the linux headers, we've defined
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
  | Missing247
  | Missing248
  | Missing249
  | Missing250
  | Missing251
  | Missing252
  | Missing253
  | Missing254
  | Missing255
#ifdef darwin_HOST_OS
  | KeyFn
  | KeyLaunchpad
  | KeyMissionCtrl
#else
  | Missing256
  | Missing257
  | Missing258
#endif
  | Missing259
  | Missing260
  | Missing261
  | Missing262
  | Missing263
  | Missing264
  | Missing265
  | Missing266
  | Missing267
  | Missing268
  | Missing269
  | Missing270
  | Missing271
  | Missing272
  | Missing273
  | Missing274
  | Missing275
  | Missing276
  | Missing277
  | Missing278
  | Missing279
  | Missing280
  | Missing281
  | Missing282
  | Missing283
  | Missing284
  | Missing285
  | Missing286
  | Missing287
  | Missing288
  | Missing289
  | Missing290
  | Missing291
  | Missing292
  | Missing293
  | Missing294
  | Missing295
  | Missing296
  | Missing297
  | Missing298
  | Missing299
  | Missing300
  | Missing301
  | Missing302
  | Missing303
  | Missing304
  | Missing305
  | Missing306
  | Missing307
  | Missing308
  | Missing309
  | Missing310
  | Missing311
  | Missing312
  | Missing313
  | Missing314
  | Missing315
  | Missing316
  | Missing317
  | Missing318
  | Missing319
  | Missing320
  | Missing321
  | Missing322
  | Missing323
  | Missing324
  | Missing325
  | Missing326
  | Missing327
  | Missing328
  | Missing329
  | Missing330
  | Missing331
  | Missing332
  | Missing333
  | Missing334
  | Missing335
  | Missing336
  | Missing337
  | Missing338
  | Missing339
  | Missing340
  | Missing341
  | Missing342
  | Missing343
  | Missing344
  | Missing345
  | Missing346
  | Missing347
  | Missing348
  | Missing349
  | Missing350
  | Missing351
  | Missing352
  | Missing353
  | Missing354
  | Missing355
  | Missing356
  | Missing357
  | Missing358
  | Missing359
  | Missing360
  | Missing361
  | Missing362
  | Missing363
  | Missing364
  | Missing365
  | Missing366
  | Missing367
  | Missing368
  | Missing369
  | Missing370
  | Missing371
  | Missing372
  | Missing373
  | Missing374
  | Missing375
  | Missing376
  | Missing377
  | Missing378
  | Missing379
  | Missing380
  | Missing381
  | Missing382
  | Missing383
  | Missing384
  | Missing385
  | Missing386
  | Missing387
  | Missing388
  | Missing389
  | Missing390
  | Missing391
  | Missing392
  | Missing393
  | Missing394
  | Missing395
  | Missing396
  | Missing397
  | Missing398
  | Missing399
  | Missing400
  | Missing401
  | Missing402
  | Missing403
  | Missing404
  | Missing405
  | Missing406
  | Missing407
  | Missing408
  | Missing409
  | Missing410
  | Missing411
  | Missing412
  | Missing413
  | Missing414
  | Missing415
  | Missing416
  | Missing417
  | Missing418
  | Missing419
  | Missing420
  | Missing421
  | Missing422
  | Missing423
  | Missing424
  | Missing425
  | Missing426
  | Missing427
  | Missing428
  | Missing429
  | Missing430
  | Missing431
  | Missing432
  | Missing433
  | Missing434
  | Missing435
  | Missing436
  | Missing437
  | Missing438
  | Missing439
  | Missing440
  | Missing441
  | Missing442
  | Missing443
  | Missing444
  | Missing445
  | Missing446
  | Missing447
  | Missing448
  | Missing449
  | Missing450
  | Missing451
  | Missing452
  | Missing453
  | Missing454
  | Missing455
  | Missing456
  | Missing457
  | Missing458
  | Missing459
  | Missing460
  | Missing461
  | Missing462
  | Missing463
  | KeyFn2
  deriving (Eq, Show, Bounded, Enum, Ord, Generic, Hashable)


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
kcAll = S.fromList $ [minBound .. maxBound]

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
  , (KeyLeftAlt,        ["lalt", "alt"])
  , (KeyRightAlt,       ["ralt"])
  , (KeyCompose,        ["comp", "cmps", "cmp"])
  , (KeyLeftShift,      ["lshift", "lshft", "lsft", "shft", "sft"])
  , (KeyRightShift,     ["rshift", "rshft", "rsft"])
  , (KeyLeftCtrl,       ["lctrl", "lctl", "ctl"])
  , (KeyRightCtrl,      ["rctrl", "rctl"])
  , (KeyLeftMeta,       ["lmeta", "lmet", "met"])
  , (KeyRightMeta,      ["rmeta", "rmet"])
  , (KeyBackspace,      ["bks", "bspc"])
  , (KeyCapsLock,       ["caps"])
  , (Key102nd,          ["102d", "lsgt", "nubs"])
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
  , (KeyBackslash,      ["bksl", "\\"]) -- NOTE: "\\" here is a 1char string, the first \ is consumed by Haskell as an escape character
  , (KeyComma,          ["comm", ","])
  , (KeyDot,            ["."])
  , (KeySlash,          ["/"])
  , (KeyNumLock,        ["nlck"])
  , (KeyKpSlash,        ["kp/"])
  , (KeyKpEnter,        ["kprt"])
  , (KeyKpPlus,         ["kp+"])
  , (KeyKpAsterisk,     ["kp*"])
  , (KeyKpMinus,        ["kp-"])
  , (KeyKpDot,          ["kp."])
  , (KeySysRq,          ["ssrq", "sys"])
  , (KeyKbdIllumDown,   ["bldn"])
  , (KeyKbdIllumUp,     ["blup"])
  , (KeyNextSong,       ["next"])
  , (KeyPlayPause,      ["pp"])
  , (KeyPreviousSong,   ["prev"])
  , (KeyFn2,            ["fn2"])
#ifdef darwin_HOST_OS
  , (KeyLaunchpad,      ["lp"])
  , (KeyMissionCtrl,    ["mctl"])
#endif
  ]
