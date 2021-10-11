module KMonad.Util.Keyboard.Keynames
  ( -- * $core
    CoreNames

    -- * $collection
  , knAll
  , knLetters
  , knNumbers
  , knPunct
  , knMods
  , knFKeys
  , knKeypad
  , knKPNums
  , knKPSymb
  , knAC
  , knLang
  , knOther
  )
where

import KMonad.Prelude
import KMonad.Util.Name
-- import KMonad.Util.Keyboard.OS
import KMonad.Util.Keyboard.Common

import qualified RIO.HashMap as M
import qualified RIO.Set     as S
import qualified RIO.Text    as T

{-
NOTE:

This is a categorized overview of all core keynames that we support. Internally
we use standard usEN qwerty, although I'm working on locale-support, but that
will happen 'at the edges' (i.e. at the parser and key-IO). The source-code
should always reflect standard US-english.

-}


-- Some helper functions to create a bunch of CoreNames

-- | Create a list of singleton-text entries from a string
t :: String -> [Text]
t = map T.singleton

-- | Turn a list of 'Text's into a CoreNames
l :: [Text] -> CoreNames
l = map CoreName

-- | Turn a string into a set of single-letter names
m :: String -> CoreNames
m = l . t


--------------------------------------------------------------------------------
-- $collection


type CoreNames = [CoreName]

-- | All 'CoreName's used in KMonad
knAll :: CoreNames
knAll = concat [ knLetters, knNumbers, knPunct, knMods
               , knFKeys, knKeypad, knOther, knAC, knLang ]

-- | All letter names
knLetters :: CoreNames
knLetters = m ['a'..'z']

-- | All number names
knNumbers :: CoreNames
knNumbers = m ['0'..'9']

-- | All punctuation names
knPunct :: CoreNames
knPunct = m ",./-=[];'`\\"

-- | All modifier names
knMods :: CoreNames
knMods = l $ foldMap f ["ctl", "alt", "met", "sft"] where
  f s = ["l" <> s, "r" <> s]

-- | First 12 function keys
knFKeys :: CoreNames
knFKeys = l $ map (("f" <>) . tshow) [1..12 :: Int]

-- | Keypad keycodes
knKPNums, knKPSymb, knKeypad :: CoreNames
knKPNums = l $ map ("kp" <>) $ t ['0'..'9']
knKPSymb = l $ map ("kp" <>) $ ["-", "+", "*", "/", ".", "rt"]
knKeypad = knKPNums <> knKPSymb


-- | Other
--
-- vague names explained:
-- bspc -> backspace
-- nlck -> numlock
-- slck -> scroll-lock
-- 102d -> on many european keyboards, first key right of left-shift
-- sys  -> SysReq or PrintScreen
-- pgdn -> pagedown
-- pgup -> pageup
-- rght -> right-arrow
-- ins  -> insert
-- del  -> delete
-- paus -> pause
-- cmps -> compose key
-- docs -> I don't know, asking @joshskidmore
knOther :: CoreNames
knOther = [ "esc", "tab", "ret", "bspc", "caps", "nlck", "slck" , "spc", "102d"
          , "sys", "pgdn", "pgup", "home", "end" , "up", "down", "left", "rght"
          , "ins", "del", "paus" , "cmps", "docs"]


-- | Application control buttons
--
knAC :: CoreNames
knAC = [ "back", "fwd" ]

-- | Language input buttons
knLang :: CoreNames
knLang = [ "zenk", "hang" ]

