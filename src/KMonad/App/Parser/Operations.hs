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



-- | Turn a hashmap of names to values into a Parser
--
-- Turn a Name-Map into a parser that can parse elements by name. This is a bit
-- tricky, because you need to retry partially-succesful matches, furthermore,
-- you need to sort the names to match longest first.
byName :: NameMap a -> Parser a
byName = choice . map go . sortBy f . M.toList where
  f  (k, _) (l, _) = compare l k    -- ^ Reverse sort by name
  go (k, v) = v <$ (try $ string k) -- ^ Match the name and insert the value

-- | Make a button that emits a particular keycode
emitOf :: Name -> DefButton
emitOf = KEmit . kc

-- | Make a button that emits a particular shifted keycode
shiftedOf :: Name -> DefButton
shiftedOf = KAround (emitOf "lsft") . emitOf

-- thing :: [(Text, Text)]
-- thing = zip $
-- thing = over (both.traversed) (+1) ([1, 2], [8, 7])
thing :: [(Text, Text)]
thing = uncurry zip $ over (both.traversed) T.singleton ("abba", "1234")
