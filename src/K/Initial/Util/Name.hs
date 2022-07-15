-- |

module K.Initial.Util.Name
  ( Name
  , Names
  , Named
  , NameMap

  , NameError(..)
  , AsNameError(..)

  , HasName(..)
  , HasNames(..)
  , HasNamed(..)

  , checkNames
  , nameFor
  , bigger
  , mkNameMap
  )

where

import K.Initial.Util.Initial

import qualified RIO.List as L
import qualified RIO.Text as T
import qualified RIO.HashMap as M
import qualified Control.Exception.Lens as Exc

-- basic types -----------------------------------------------------------------

type Name = Text                  -- ^ Text in its function as a name
type Names = [Name]               -- ^ Multiple names
type Named a = [(Name, a)]        -- ^ An a-list of name-value pairs
type NameMap a = M.HashMap Name a -- ^ A hashmap of named things

-- errors ----------------------------------------------------------------------

-- | Things that can go wrong with 'Name' or 'Names'
data NameError
  = EmptyName            -- ^ Encountered an empty 'Name'
  | DuplicateNames Names -- ^ Encountered duplicate 'Names'
  | NoSuchName Name      -- ^ Tried to access a nonexistent 'Name'
  deriving Eq
makeClassyPrisms ''NameError

instance Show NameError where
  show EmptyName = "Encountered an empty <Name>"
  show (DuplicateNames ns) = "Encountered duplicate names: "
    <> (unpack . T.intercalate ", " . map tshow $ ns)
  show (NoSuchName n) = "Could not find <Name>: " <> unpack n

instance Exception NameError
instance AsNameError SomeException where _NameError = Exc.exception

type CanNameError e m = (MonadError e m, AsNameError e)

-- lenses ----------------------------------------------------------------------

class HasName a where name :: Lens' a Name
class HasNames a where names :: Fold a Name
class HasNamed s a where named :: Getter s (Named a)

instance HasName Name where name = id

instance HasNames Name where names = id
instance HasNames Names where names = folded
instance HasNames (Named a) where names = folded . _1
instance HasNames (NameMap a) where names = to M.keys . folded

instance HasNamed (Named a) a where named = id
instance HasNamed (NameMap a) a where named = to M.toList

-- ops -------------------------------------------------------------------------

-- | Check all names for validity
checkNames :: (CanNameError e m, HasNames a) => a -> m ()
checkNames a = do
  let ns = a^..names
  whenJust (L.find T.null ns)  $ \_ -> throwError $ _EmptyName # ()
  whenNonEmpty (duplicates ns) $ \d -> throwError $ _DuplicateNames # d

-- | Do a reverse-lookup for the name of some item
nameFor :: (HasNamed s a, Eq a) => a -> s -> Maybe Name
nameFor v = lookup v . map (view swapped) . view named

-- | Compare 2 names first on length, and if equal, alphabetically
bigger :: Name -> Name -> Ordering
bigger a b = case compare (T.length b) (T.length a) of EQ -> compare a b
                                                       x  -> x

-- | Turn a Named into a NameMap, but error on empty or duplicate names
mkNameMap :: CanNameError e m => Named a -> m (NameMap a)
mkNameMap x = do
  case L.find T.null $ x^..names of
    Nothing -> pure ()
    Just _  -> errThrowing _EmptyName ()
  case duplicates $ x^..names of
    [] -> pure ()
    ns -> errThrowing _DuplicateNames ns
  pure $ M.fromList x

-- | Insert aliases for some name into a NameMap
--
-- NOTE: This is more general than 'Name', and might be a map-utility (or 'At'
-- utility) instead. However, the NameError lines up very well with what can go
-- wrong (maybe it's more of a KeyError...), and I only ever use aliases in the
-- context of names, so for KMonad, this is a practical place to keep this
-- operation.
-- addAliases :: CanNameError e m => [(Name, [Name])] -> NameMap a -> m (NameMap a)
-- addAliases as m = do
