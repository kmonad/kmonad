{-|
Module      : KMonad.Util.MultiMap
Description : A `k -> Set v` mapping, with reversing utilities
Copyright   : (c) David Janssen, 2019
License     : MIT
Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : portable

This datastructure represents a `k -> Set v` mapping: that is to say, each key
can have multiple values (but no duplicates). Additionally, we provide some
operations to reverse this mapping.

In KMonad we use this exclusively to easily define multiple names for the same
'KMonad.Keyboard.Keycode' in a reversible manner.

-}
module KMonad.Util.MultiMap
  ( -- * Types
    -- $typ
    MultiMap
  , mkMultiMap
  , fromSingletons

    -- * Operations on MultiMaps
    -- $ops
  , itemed
  , reverse
  )
where

import KMonad.Prelude hiding (reverse)

import qualified RIO.HashMap as M
import qualified RIO.HashSet as S

--------------------------------------------------------------------------------
-- $typ

-- | All the type constraints required for something to function as a MultiMap
type CanMM k v = (Eq k, Ord v, Hashable k, Hashable v)

-- | The 'MultiMap', which describes a one to many (unique) mapping
newtype MultiMap k v = MultiMap { _unMM :: M.HashMap k (S.HashSet v) }
  deriving Show
makeLenses ''MultiMap

instance (CanMM k v) => Semigroup (MultiMap k v) where
  (MultiMap a) <> (MultiMap b) = MultiMap $ M.unionWith (<>) a b
instance (CanMM k v) => Monoid (MultiMap k v) where
  mempty = MultiMap M.empty

type instance Index   (MultiMap k v) = k
type instance IxValue (MultiMap k v) = S.HashSet v

instance CanMM k v => Ixed (MultiMap k v)
instance CanMM k v => At (MultiMap k v) where
  at k = unMM . at k

-- | Create a new multimap from a foldable of (k, foldable v) pairs.
mkMultiMap :: (Foldable t1, Foldable t2, CanMM k v)
  => t1 (k, t2 v) -> MultiMap k v
mkMultiMap = foldMap
  ( MultiMap
  . uncurry M.singleton
  . over _2 (S.fromList . toList)
  )

-- | Create a new multimap from a foldable of (k, v) pairs
fromSingletons :: (Foldable t, CanMM k v)
  => t (k, v) -> MultiMap k v
fromSingletons = mkMultiMap . map (over _2 (:[])) . toList



--------------------------------------------------------------------------------
-- $ops

-- | A fold over all the (k, v) pairs in a 'MultiMap'
itemed :: (CanMM k v) => Fold (MultiMap k v) (k, v)
itemed = folding $ \m -> m ^@.. unMM . ifolded <. folded

-- | Reverse a MultiMap. Note: this is not necessarily a lossless conversion.
reverse :: (CanMM k v, CanMM v k) => MultiMap k v -> MultiMap v k
reverse m = mkMultiMap $ m ^.. itemed . swapped . to (over _2 (:[]))
