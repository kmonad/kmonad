{-# OPTIONS_HADDOCK not-home #-}
{-|
Module      : KMonad.Core.Parser.Parsers.KeyCode
Description : How to parse keycodes
Copyright   : (c) David Janssen, 2019
License     : MIT
Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : portable

Keycodes are mainly parsed by names.

-}
module KMonad.Core.Parser.Parsers.KeyCode
  ( keycodeP
  , module KMonad.Core.KeyCode
  )
where

import Control.Arrow ((&&&))
import Data.Foldable (foldl')

import KMonad.Core.KeyCode
import KMonad.Core.Parser.Utility

import qualified Data.Text as T


--------------------------------------------------------------------------------
-- Parsing keycodes by its name

-- | Parse a 'KeyCode'
keycodeP :: Parser KeyCode
keycodeP = fromNamed allNames

-- | A large alist of all named 'KeyCode's
allNames :: Named KeyCode
allNames = singleChar <> lowerNames <> strippedNames <> specialNames

-- | Single-character identifiers of 'KeyCode's
singleChar :: Named KeyCode
singleChar =
  [ ("-",  KeyMinus)
  , ("=",  KeyEqual)
  , ("[",  KeyLeftBrace)
  , ("]",  KeyRightBrace)
  , (";",  KeySemicolon)
  , ("'",  KeyApostrophe)
  , ("`",  KeyGrave)
  , ("\\", KeyBackslash)
  , (",",  KeyComma)
  , (".",  KeyDot)
  , ("/",  KeySlash)
  ]

-- | Lower-cased shows of 'KeyCode' names
lowerNames :: Named KeyCode
lowerNames = map (T.toLower . T.pack . show &&& id) $ [minBound .. maxBound]

-- | Names with the prefix key stripped off
strippedNames :: Named KeyCode
strippedNames = foldl' (\acc (k, v) -> case T.stripPrefix "key" k of
                           Just k' -> (k', v):acc
                           Nothing -> acc)
                [] lowerNames

-- | Various shortcuts
specialNames :: Named KeyCode
specialNames = concatMap (\(k, as) -> map (,k) as) $
  [ (KeyEnter,          ["ret", "return", "ent"])
  , (KeyRight,          ["rght"])
  , (KeyMinus,          ["min"])
  , (KeyEqual,          ["eql"])
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
  ]
