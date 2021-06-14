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
keycodeP :: Parser Keycode
keycodeP = fromLexicon allNames <?> "keycode" where
  -- Lexicon of keycode names including aliases
  allNames  = case foldM (flip insertAlias) coreNames keycodeAliases of
    Left err  -> throw err
    Right lex -> lex
  -- Lexicon of core keycode names as text
  coreNames = M.fromList . map (first unCore) . M.toList $ keycodeNames

-- | A collection of alternative names for existing 'CoreName's
--
-- NOTE: Our unit tests guarantee that:
-- * This list only provides aliases to valid 'CoreName's
-- * There are no duplicate aliases
-- * No alias overlaps with a 'CoreName'
--
keycodeAliases :: [Alias]
keycodeAliases = foldMap (\(c, as) -> map (,unCore c) as) $
  [ ("ret" , ["return", "ent", "enter"])
  , ("-"   , ["min", "minus"])
  , ("="   , ["eql", "equal"])
  , ("spc" , ["space", "spacebar"])
  , ("pgup", ["pageup"])
  , ("pgdn", ["pagedown"])
  , ("ins" , ["insert"])
  , ("del" , ["delete"])
  , ("cmps", ["compose", "cmp"])
  , ("lsft", ["sft", "shft", "lshft", "lshift", "leftshift"])
  , ("rsft", ["rshft", "rshift", "rightshift"])
  , ("lalt", ["alt", "leftalt"])
  , ("ralt", ["rightalt"])
  , ("lctl", ["ctl", "ctrl", "lctrl", "lcontrol", "leftcontrol"])
  , ("rctl", ["rctrl", "rcontrol", "rightcontrol"])
  , ("lmet", ["met", "meta", "lmeta", "leftmeta"])
  , ("rmet", ["rmeta", "rightmeta"])
  , ("bspc", ["backspace", "bckspc", "bspace", "bks"])
  , ("caps", ["cplk", "capslock", "CAPS"])
  , ("102d", ["102nd", "102", "lsgt", "nubs"])
  , ("slck", ["scrl", "scrolllock", "slock", "scroll"])
  , ("left", ["lft", "<-"])
  , ("rght", ["right", "->"])
  , ("["   , ["lbrc", "leftbrace", "lbrace"])
  , ("]"   , ["rbrc", "rightbrace", "rbrace"])
  , (";"   , ["scln", "semi", "semicolon"])
  , ("'"   , ["apos", "apo", "apostrophe"])
  , ("`"   , ["grv", "grave"])
  , ("\\"  , ["bksl", "backslash", "bslash"]) -- NOTE: "\\" is a 1-char string (escaped backslash)
  , (","   , ["comm", "comma"])
  , ("."   , ["period", "dot"])
  , ("/"   , ["sl", "slsh", "slash"])
  , ("nlck", ["num", "numlock"])
  , ("kp/" , ["kpsl", "kpslash"])
  , ("kprt", ["kpret", "kpenter"])
  , ("kp+" , ["kpplus"])
  , ("kp*" , ["kptimes", "kpasterisk"])
  , ("kp-" , ["kpminus", "kpmin"])
  , ("kp." , ["kpdot"])
  , ("sys" , ["ssrq", "psrc", "printscreen"])
  , ("paus", ["pause"])
  ] <> fkeyAliases

fkeyAliases :: [(CoreName, [Text])]
fkeyAliases = map go [1..12] where
  go i = let c = tshow i in (CoreName $ "f" <> c, ["F" <> c])



--------------------------------------------------------------------------------
-- $delete
--
-- Stuff that we should clean up when we're finished with the keycode refactor.
-- Basically, aliases for names that we haven't decided how to handle yet, but
-- included, since I was typing anyways.

-- | This is a list of aliases for keycodes that we haven't given a CoreName
-- yet, since they don't exist on a standard 100% keyboard.
nonExistent :: [Alias]
nonExistent = foldMap (\(c, as) -> map (,unCore c) as)
  [ -- FIXME: Hookup these aliases to rare Linux names
    ("slp" , ["sleep", "zzz"])
  , ("volu", ["volumeup", "loud", "louder"])
  , ("vold", ["volumedown", "soft", "softer"])
  , ("brgu", ["brightnessup", "brigtup", "light", "bright", "lght"])
  , ("brgd", ["brightnessdown", "brigtdn", "dim", "dark"])
  , ("fwd" , ["forward"])
  , ("prnt", ["print"])
  , ("wku" , ["wakeup", "wake", "wkup"])
  , ("illd", ["illumdown", "kbillumdown", "kbdim"])
  , ("illu", ["illumup", "kbillumup", "kbbr"])
  , ("next", ["nextsong"])
  , ("pp"  , ["playpause", "songpause"])
  , ("prev", ["prevsong", "previoussong"])

  -- FIXME: Hookup these aliases for mac-names
  , ("pad" , ["launchpad", "launch"])
  , ("mctl", ["missioncttrl", "mctrl"])
  ]
