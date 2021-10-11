module KMonad.App.Parser.Operations

where

import KMonad.Prelude hiding (try)

import KMonad.App.Parser.Types
import KMonad.Util.Name
import KMonad.Util.Keyboard

import RIO.List (sortBy)
import qualified RIO.HashMap as M
import qualified RIO.Text as T

import Text.Megaparsec (choice, try)
import Text.Megaparsec.Char (string)

-- | Like 'fromLexicon' but case-insensitive
fromLexicon' :: Lexicon a -> Parser a
fromLexicon' = choice . map go . sortBy f . M.toList where
  f  (k, _) (l, _) = compare l k     -- ^ Reverse sort by name
  go (k, v) = v <$ (try $ string' k) -- ^ Match the name and insert the value

-- | Turn a Lexicon of names items into a parser
fromLexicon :: Lexicon a -> Parser a
fromLexicon = choice . map go . sortBy f . M.toList where
  f  (k, _) (l, _) = compare l k    -- ^ Reverse sort by name
  go (k, v) = v <$ (try $ string k) -- ^ Match the name and insert the value

-- | Make a button that emits a particular keycode
emitOf :: CoreName -> DefButton
emitOf = KEmit . kc

-- | Make a button that emits a particular shifted keycode
shiftedOf :: CoreName -> DefButton
shiftedOf = KAround (emitOf "lsft") . emitOf

-- thing :: [(Text, Text)]
-- thing = zip $
-- thing = over (both.traversed) (+1) ([1, 2], [8, 7])
thing :: [(Text, Text)]
thing = uncurry zip $ over (both.traversed) T.singleton ("abba", "1234")
