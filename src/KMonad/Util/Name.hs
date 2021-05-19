{- Some utilities for working with names of things -}
module KMonad.Util.Name

where

import KMonad.Prelude

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


-- | Add references for all objects in a by their aliases
--
-- In case an alias refers to a key that doesn't occur in the map, it gets
-- skipped. In case an alias tries to overwrite a name in the original map, that
-- single alias gets skipped.
insertAliases :: [Alias] -> NameMap a -> NameMap a
insertAliases als nms = foldr go nms als where
  go (k, ls) m = case M.lookup k m of
    Just a  -> m `M.union` M.fromList (map (,a) ls)
    Nothing -> m

