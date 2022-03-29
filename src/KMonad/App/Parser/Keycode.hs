module KMonad.App.Parser.Keycode
  ( -- * $parser
    keycodeP

    -- * $aliases
  , keycodeAliases
  )
where

import KMonad.Prelude
import KMonad.Util.Keyboard
import KMonad.Util.Name
import KMonad.App.Parser.Operations
import KMonad.App.Parser.Types

import qualified RIO.HashMap as M
import qualified RIO.Text as T

--------------------------------------------------------------------------------
-- $parser
--
-- The core parser that reads keycodes from raw-text. This is defined as a large
-- hashmap that is constructed by adding a bunch of aliases on top of the
-- collection of core names.
--
-- NOTE: These aliases only exist within the scope of parsing. Once the config
-- has been parsed, we only refer to 'Keycode's by their 'CoreName'. In the
-- future we might add support for locales, at which point we will add that
-- remapping separately.

-- | Parse a keycode
--
-- Parse a keycode by either its corename, an alias, or either a corename or
-- alias preceded by "key"
--
keycodeP :: Parser Keycode
keycodeP = nameP
  where
    -- keynameP = string' "key" *> nameP
    nameP = fromLexicon allNames <?> "keycode"
    -- Lexicon of core keycode names as text
    coreNames = M.fromList . map (first unCore) . M.toList $ keycodeNames
    -- Lexicon of keycode names including aliases
    allNames  = case foldM (flip insertAlias) coreNames keycodeAliases of
      Left err  -> throw err
      Right lex -> lex

keycodeAliases :: [(Text, Text)]
keycodeAliases = foldMap go $ manualAliases <> keyKeyAliases <> fkeyAliases
  where go (c, as) = map (,unCore c) as

-- | A collection of alternative names for existing 'CoreName's
--
-- NOTE: Our unit tests guarantee that:
-- * This list only provides aliases to valid 'CoreName's
-- * There are no duplicate aliases
-- * No alias overlaps with a 'CoreName'
--
manualAliases :: [(CoreName, [Text])]
manualAliases =
  [ ("ret" , ["return", "ent", "entr", "enter", "KeyEnter"])
  , ("-"   , ["min", "minus", "KeyMinus"])
  , ("="   , ["eql", "equal", "KeyEqual"])
  , ("spc" , ["space", "spacebar", "KeySpace"])
  , ("pgup", ["pageup", "KeyPageUp"])
  , ("pgdn", ["pagedown", "KeyPageDown"])
  , ("ins" , ["insert", "KeyInsert"])
  , ("del" , ["delete", "KeyDelete"])
  , ("lsft", ["sft", "shft", "lshft", "lshift", "leftshift", "KeyLeftShift"])
  , ("rsft", ["rshft", "rshift", "rightshift", "KeyRightShift"])
  , ("lalt", ["alt", "leftalt", "KeyLeftAlt"])
  , ("ralt", ["rightalt", "KeyRightAlt"])
  , ("lctl", ["ctl", "ctrl", "lctrl", "lcontrol", "leftcontrol", "KeyLeftControl"])
  , ("rctl", ["rctrl", "rcontrol", "rightcontrol", "KeyRightControl"])
  , ("lmet", ["met", "meta", "lmeta", "leftmeta", "KeyLeftMeta"])
  , ("rmet", ["rmeta", "rightmeta", "KeyRightMeta"])
  , ("bspc", ["backspace", "bckspc", "bspace", "bks", "KeyBackspace"])
  , ("caps", ["cplk", "capslock", "CAPS", "KeyCapsLock"])
  , ("slck", ["scrl", "scrolllock", "slock", "scroll", "KeyScrollLock"])
  , ("left", ["lft", "<-", "KeyLeft"])
  , ("rght", ["right", "->", "KeyRight"])
  , ("["   , ["lbrc", "leftbrace", "lbrace", "KeyLeftBrace"])
  , ("]"   , ["rbrc", "rightbrace", "rbrace", "KeyRightBrace"])
  , (";"   , ["scln", "semi", "semicolon", "KeySemicolon"])
  , ("'"   , ["apos", "apo", "apostrophe", "KeyApostrophe"])
  , ("`"   , ["grv", "grave", "KeyGrave"])
  , ("\\"  , ["bksl", "backslash", "bslash", "KeyBackslash"]) -- NOTE: "\\" is a 1-char string (escaped backslash)
  , (","   , ["comm", "comma", "KeyComma"])
  , ("."   , ["period", "dot", "KeyDot"])
  , ("/"   , ["sl", "slsh", "slash", "KeySlash"])
  , ("nlck", ["num", "numlock", "KeyNumLock"])
  , ("kp/" , ["kpsl", "kpslash", "KeyKpSlash"])
  , ("kprt", ["kpret", "kpenter", "KeyKpEnter"])
  , ("kp+" , ["kpplus", "KeyKpPlus"])
  , ("kp*" , ["kptimes", "kpasterisk", "KeyKpAsterisk"])
  , ("kp-" , ["kpminus", "kpmin", "KeyKpMin"])
  , ("kp." , ["kpdot", "KeyKpDot"])
  , ("sys" , ["ssrq", "psrc", "printscreen", "KeySysRq"])
  , ("paus", ["pause", "KeyPause"])
  , ("cmps", ["compose", "cmp", "nUS\\", "KeyCompose"])
  , ("102d", ["102nd", "102", "lsgt", "nubs", "app", "Key102nd"])
--  , ("docs",  ["documents", "dcs", "KeyDocuments"])
--  , ("zenk", ["zenkaku", "KeyZenkakuHankaku", "zenkakuhankaku", "hank"])
--  , ("hang", ["hangeul", "hanguel", "KeyHangeul", "KeyHanguel"])
  ]

-- | F11 and KeyF11 for f11 etc.
fkeyAliases :: [(CoreName, [Text])]
fkeyAliases = map go knFKeys
  where go c = let x = capitalize (unCore c) in (c, [x, "Key" <> x])

-- | KeyXX names for all numbers and letters
keyKeyAliases :: [(CoreName, [Text])]
keyKeyAliases = map go $ knLetters <> knNumbers
  where go c = (c, ["Key" <> T.toUpper (unCore c)])


--------------------------------------------------------------------------------
-- $delete
--
-- Stuff that we should clean up when we're finished with the keycode refactor.
-- Basically, aliases for names that we haven't decided how to handle yet, but
-- included, since I was typing anyways.

-- | This is a list of aliases for keycodes that we haven't given a CoreName
-- yet, since they don't exist on a standard 100% keyboard.
-- nonExistent :: [Alias]
-- nonExistent = foldMap (\(c, as) -> map (,unCore c) as)
--   [ -- FIXME: Hookup these aliases to rare Linux names
--     ("slp" , ["sleep", "zzz"])
--   , ("volu", ["volumeup", "loud", "louder"])
--   , ("vold", ["volumedown", "soft", "softer"])
--   , ("brgu", ["brightnessup", "brigtup", "light", "bright", "lght"])
--   , ("brgd", ["brightnessdown", "brigtdn", "dim", "dark"])
--   , ("fwd" , ["forward"])
--   , ("prnt", ["print"])
--   , ("wku" , ["wakeup", "wake", "wkup"])
--   , ("illd", ["illumdown", "kbillumdown", "kbdim"])
--   , ("illu", ["illumup", "kbillumup", "kbbr"])
--   , ("next", ["nextsong"])
--   , ("pp"  , ["playpause", "songpause"])
--   , ("prev", ["prevsong", "previoussong"])

--   -- FIXME: Hookup these aliases for mac-names
--   , ("pad" , ["launchpad", "launch"])
--   , ("mctl", ["missioncttrl", "mctrl"])
--   ]
