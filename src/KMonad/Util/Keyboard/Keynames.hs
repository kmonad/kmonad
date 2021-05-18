module KMonad.Util.Keyboard.Keynames
  ( Keynames
  , knAll
  , knLetters
  , knNumbers
  , knPunct
  , knMods
  , knFKeys
  , knKeypad
  , knOther
  )
where

import KMonad.Prelude
import KMonad.Util.Keyboard.Types

import qualified RIO.Set  as S
import qualified RIO.Text as T

{-
NOTE:

This is a categorized overview of all core keynames that we support. Internally
we use standard usEN qwerty, although I'm working on locale-support, but that
will happen 'at the edges' (i.e. at the parser and key-IO). The source-code
should always reflect standard US-english.

-}

type Keynames = S.Set Keyname

-- | Create a list of singleton-text entries from a string
t :: String -> [Text]
t = map T.singleton

-- | Turn a string into a set of single-letter names
m :: String -> Keynames
m = S.fromList . t

knAll :: Keynames
knAll = S.unions [knLetters, knNumbers, knPunct, knMods
                 , knFKeys, knKeypad, knOther ]

-- | All letter names
knLetters :: Keynames
knLetters = m ['a'..'z']

-- | All number names
knNumbers :: Keynames
knNumbers = m ['0'..'9']

-- | All punctuation names
knPunct :: Keynames
knPunct = m ",./-=[];'`\\"

-- | All modifier names
knMods :: Keynames
knMods = S.fromList $ foldMap f ["ctl", "alt", "met", "sft"] where
  f s = ["l" <> s, "r" <> s]

-- | First 12 function keys
knFKeys :: Keynames
knFKeys = S.fromList $ map (("f"<>) . tshow) [1..12 :: Int]

-- | Names for keypad buttons
--
-- vague names explained:
-- kprt -> keypad return
knKeypad :: Keynames
knKeypad = S.fromList $ map f
  $ t ['0'..'9'] <> ["-", "+", "*", "/", ".", "rt"]
  where f s = "kp" <> s

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
knOther :: Keynames
knOther = S.fromList [ "esc", "tab", "ret", "bspc", "caps", "nlck", "slck"
                     , "spc", "102d", "sys", "pgdn", "pgup", "home", "end"
                     , "up", "down", "left", "rght", "ins", "del" ]
