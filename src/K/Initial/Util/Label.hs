{- |
Labels are just Text, and Labeled is just a HashMap. However, we use them in a
slightly more specific way, so we gather some operations here to make using them
a bit easier.

The differences:
- many keys to single value mapping (aliases)
- reverse lookup
- don't allow key-overwriting
- slightly more informative errors
-}

{- Notes to self:

The benefit of throwing lookup errors (instead of returning Nothing) is that we
can do many lookups in some fold over a large structure and we don't have to
keep track of exactly where we are. If a lookup fails anywhere, it will bubble
up, with the offending label.

-}

module K.Initial.Util.Label

where

import K.Initial.Util.Initial

import qualified RIO.List as L
import qualified RIO.HashMap as M
import qualified RIO.Text as T


-- basic types -----------------------------------------------------------------

-- | A label used to assign some arbitrary name to a value
type Label = Text

-- | A map from label to item
type Labeled = M.HashMap Label

-- | A map from item to all its labels
type LabelsFor a = M.HashMap a [Label]

-- errors ----------------------------------------------------------------------

-- | An error thrown when trying to lookup a non-existent key
newtype LookupError = NoSuchLabel Label

-- | An error thrown when trying to insert an existing key
newtype InsertError = DuplicateLabel Label

-- | Either an insertion or lookup error
data LabelError
  = LabelLookupError LookupError
  | LabelInsertError InsertError

makeClassyPrisms ''LookupError
makeClassyPrisms ''InsertError
makeClassyPrisms ''LabelError

instance Show LookupError where
  show (NoSuchLabel l) = "Nonexistent label: '" <> unpack l <> "'"
instance Show InsertError where
  show (DuplicateLabel l) = "Duplicate label: '" <> unpack l <> "'"
instance Show LabelError where
  show (LabelLookupError e) = show e
  show (LabelInsertError e) = show e

instance Exception LookupError
instance Exception InsertError
instance Exception LabelError

instance AsLookupError LabelError where _LookupError = _LabelLookupError
instance AsInsertError LabelError where _InsertError = _LabelInsertError

-- ops -------------------------------------------------------------------------

-- | Lookup the value for a 'Label' in a 'Labeled'
lookup :: (MonadError e m, AsLookupError e) => Label -> Labeled a -> m a
lookup l m = case M.lookup l m of
  Nothing -> errThrowing _NoSuchLabel l
  Just x -> pure x

-- | Create a 'Labeled' from an alist of labels to values.
--
-- Will throw an InsertError when a label occurs more than once.
fromPairs :: (MonadError e m, AsInsertError e) => [(Label, a)] -> m (Labeled a)
fromPairs ls = do
  case duplicates $ ls^..folded._1 of
    []  -> pure $ M.fromList ls
    x:_ -> errThrowing _DuplicateLabel x

-- | Create a sorted alist of (label, value) pairs
--
-- NOTE: The alist is sorted descending on label length, making it suitable for
-- immediate use in the labeledP parser in 'K.Initial.Util.Parsing'
toPairs :: Labeled a -> [(Label, a)]
toPairs = L.sortBy (bigger `on` fst) . M.toList
  where bigger a b = case compare (T.length a) (T.length b) of
          EQ -> compare a b
          x  -> x

-- | Create a map from value to list-of-labels
toLabelsFor :: (Eq a, Hashable a) => Labeled a -> LabelsFor a
toLabelsFor = foldl' f M.empty . M.toList
  where f acc (l, v) = M.insertWith (<>) v [l] acc

-- | Create a 'Labeled' from a 'LabelsFor'
--
-- Will throw an 'InsertError' on any duplicate label
fromLabelsFor :: (MonadError e m, AsInsertError e) => LabelsFor a -> m (Labeled a)
fromLabelsFor = fromPairs . toListOf (folded.swapped) . alistFlatten . M.toList

-- | Add new keys mapping to existing values in a map.
--
-- Will throw:
-- - An 'InsertError' when any label (original or alias) is a duplicate
-- - A 'LookupError' when trying to add aliases to a non-existent label
addAliases :: (MonadError e m, AsLookupError e, AsInsertError e)
  => Labeled [Label] -- ^ A label-to-aliases mapping
  -> Labeled a       -- ^ A label-to-anything mapping
  -> m (Labeled a)   -- ^ The label-to-anything mapping with aliases inserted
addAliases as m = do
  -- Build an [(alias, value)] alist
  let f (l, a) = case M.lookup l m of
        Nothing -> errThrowing _NoSuchLabel l
        Just v -> pure (a, v)
  xs <- mapM f . alistFlatten . M.toList $ as

  -- Make a new 'Labeled' from the concat of the aliases and the original list
  fromPairs $ M.toList m <> xs
