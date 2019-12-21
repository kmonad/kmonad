{-|
Module      : KMonad.Types.MapStack
Description : Implementation of a set of labeled maps in a stack
Copyright   : (c) David Janssen, 2019
License     : MIT
Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : portable

The MapStack object is designed to handle a number of different labeled maps in
a way that allows overlaying maps on top of eachother.
-}
module KMonad.Types.MapStack
  ( -- * The type and constructor for a MapStack
    -- $types
    MapStack
  , CanMS
  , mkMapStack

    -- * Manipulate MapStack objects
    -- $utils
  , push, pop, lookup, purge
  )
where

import KMonad.Prelude hiding (lookup)

import qualified RIO.HashMap as M
import qualified RIO.HashSet as S

--------------------------------------------------------------------------------
-- $types
--
-- 'MapStack' data essentially manages a number of different maps between two
-- datatypes, and maintains a stack of maps to use. When looking something up in
-- a mapstack, we first attempt to find it in the top layer, if not, then the
-- next, if not, then the next, andsoforth.

-- | The constraints required of the 'MapStack' types
type CanMS ik mk = (Eq ik, Hashable ik, Eq mk, Hashable mk)

-- | A MapStack object with a map-key, item-key, and value.
--
-- Note: we don't export the constructor and only guarantee the API, this
-- implementation is subject to unexpected changes.
data MapStack mk ik v = MapStack
  { _stack :: ![mk]
  , _maps  :: !(S.HashSet  mk)
  , _items :: !(M.HashMap (mk, ik) v)
  } deriving Show
makeLenses ''MapStack

-- | Create a 'MapStack' from a list of a list of items. It is possible to have
-- duplicate keys, but we make no guarantees on which keyed item will be
-- represented, so this is strongly discouraged. The 'MapStack' starts with an
-- empty queue, so any attempt to immediately look something up will fail.
mkMapStack :: CanMS ik mk
  => [(mk, [(ik, v)])] -- ^ An alist of mapkey to alist of itemkey to item.
  -> MapStack mk ik v
mkMapStack items' = MapStack
  { _stack = []
  , _maps  = S.fromList $ map fst items'
  , _items = foldl' fOuter M.empty items' }
  where
    fOuter    acc (mk, xs) = foldl' (fInner mk) acc xs
    fInner mk acc (ik, v)  = M.insert (mk, ik) v acc


--------------------------------------------------------------------------------
-- $utils

-- | Return the first match found by descending into the 'MapStack'
lookup :: CanMS ik mk => ik -> MapStack mk ik v -> Maybe v
lookup ik ms = go $ ms^.stack
  where
    go []     = Nothing
    go (x:xs) = maybe (go xs) Just (M.lookup (x, ik) $ ms^.items)

-- | Return a new 'MapStack' with the provided map-key added to the top stack.
-- If the map-key does not exist in the 'MapStack', return 'Nothing'.
push :: CanMS ik mk => mk -> MapStack mk ik v -> Maybe (MapStack mk ik v)
push mk ms | S.member mk $ ms^.maps = Just $ ms & stack %~ (mk <|)
           | otherwise              = Nothing

-- | Return a new 'MapStack' with the first occurence of the provided map-key
-- removed from stack. If the key cannot be found in the stack, then return
-- 'Nothing'.
pop :: CanMS ik mk => mk -> MapStack mk ik v -> Maybe (MapStack mk ik v)
pop mk ms = case break (== mk) (ms^.stack) of
  (_, [])  -> Nothing
  (h, _:t) -> Just $ ms & stack .~ (h <> t)

-- | Return a new 'MapStack' with all occurences of the provided map-key removed
-- from the stack.
purge :: CanMS ik mk => mk -> MapStack mk ik v -> MapStack mk ik v
purge mk ms = ms & stack %~ (filter (/= mk))
