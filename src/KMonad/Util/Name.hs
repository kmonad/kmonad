module KMonad.Util.Name
  ( -- * Types
    Name
  , Alias
  , NameMap
  , Aliases
  , HasName(..)

    -- * Utilities
  , fromNamed
  , insertAliases
  , byName
  , reverseMap
  )

where

import KMonad.Prelude hiding (try)
import KMonad.Types

import Text.Megaparsec (choice, try)
import Text.Megaparsec.Char (string)
import RIO.List (sortBy)

import qualified RIO.HashMap as M



-- | When we talk about names, we use text
type Name = Text

-- | Aliases are rereferences from names to other names
type Alias = (Name, [Name])

-- | Maps of names to things
type NameMap a = M.HashMap Name a
type Aliases = M.HashMap Name [Name]

class HasName a where name :: Lens' a Name

instance HasName Name where name = id
instance HasName Alias where name = _1

fromNamed :: (Foldable t, HasName a) => t a -> NameMap a
fromNamed = M.fromList . map (\a -> (a^.name, a)) . toList

-- | The real reason this module exists
--
-- Turn a Name-Map into a parser that can parse elements by name. This is a bit
-- tricky, because you need to retry only partially succesful matches,
-- furthermore, you need to sort the names to match longest first.
byName :: NameMap a -> Parser a
byName = choice . map go . sortBy f . M.toList where
  f  (k, _) (l, _) = compare l k
  go (k, v) = v <$ (try $ string k)

-- | Add references for all objects in a by their aliases
--
-- In case an alias refers to a key that doesn't occur in the map, it gets
-- skipped. In case an alias tries to overwrite a name in the original map, that
-- single alias gets skipped.
--
-- NOTE: This is used to generate parsers for named objects that have aliases.
-- Maybe making this a general function is redundant, or even confusing.
insertAliases :: [Alias] -> NameMap a -> NameMap a
insertAliases als nms = foldr go nms als where
  go (k, ls) m = case M.lookup k m of
    Just a  -> m `M.union` M.fromList (map (,a) ls)
    Nothing -> m

-- | Reverse a map
--
-- NOTE: This might overwrite duplicates.
reverseMap :: (Eq a, Eq b, Hashable a, Hashable b)
  => M.HashMap a b -> M.HashMap b a
reverseMap = M.fromList . toListOf (folded . swapped) . M.toList
