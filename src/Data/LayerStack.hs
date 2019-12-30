{-# LANGUAGE ScopedTypeVariables #-}
{-|
Module      : Data.LayerStack
Description : A container of overlapping mappings
Copyright   : (c) David Janssen, 2019
License     : MIT
Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : portable

A 'LayerStack' is a set of different mappings between keys and values, and
provides functionality for keeping track of a `stack` of these mappings. Lookup
in a 'LayerStack' happens by checking the front-most mapping on the stack, and
if that fails, descending deeper.

A 'LayerStack' has 3 type parameters, in the documentation we will refer to
those as:
  - l: The layer key, which is the identifier for the different layers
  - k: The item key, which is the per-layer identifier for different items
  - a: The item (value), which is the value stored for k in a particular layer
-}
module Data.LayerStack
  ( -- * Basic types
    -- $types
    Layer
  , mkLayer
  , LayerStack
  , mkLayerStack
  , items

    -- * Basic operations on LayerStacks
    -- $ops
  , atKey
  , pushLayer
  , popLayer

    -- * Things that can go wrong with LayerStacks
    -- $err
  , LayerStackError
  , AsLayerStackError(..)
  )

where

import Prelude

import RIO.List (delete)

import qualified RIO.HashMap as M
import qualified RIO.HashSet as S

--------------------------------------------------------------------------------
-- $err

-- | The things that can go wrong with a 'LayerStack'
data LayerStackError l
  = LayerDoesNotExist l   -- ^ Requested use of a non-existing layer
  | LayerNotOnStack   l   -- ^ Requested use of a non-stack layer
  deriving Show
makeClassyPrisms ''LayerStackError

instance (Typeable l, Show l) => Exception (LayerStackError l)


--------------------------------------------------------------------------------
-- $constraints

-- | The type of things that can function as either layer or item keys in a
-- LayerStack.
type CanKey k = (Eq k, Hashable k)

--------------------------------------------------------------------------------
-- $types

-- | A 'Layer' is one of the maps contained inside a 'LayerStack'
newtype Layer k a = Layer { unLayer :: M.HashMap k a}
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

-- | Create a new 'Layer' from a 'Foldable' of key-value pairs
mkLayer :: (Foldable t, CanKey k) => t (k, a) -> Layer k a
mkLayer = Layer . M.fromList . toList

-- | A 'LayerStack' is a named collection of maps and a sequence of maps to use
-- for lookup.
data LayerStack l k a = LayerStack
  { _stack :: ![l]                  -- ^ The current stack of layers
  , _maps  :: !(S.HashSet l)        -- ^ A set of all 'Layer' names
  , _items :: !(M.HashMap (l, k) a) -- ^ The map of all the bindings
  } deriving (Show, Eq, Functor)
makeLenses ''LayerStack

-- | Create a new 'LayerStack' from a foldable of foldables.
mkLayerStack :: (Foldable t1, Foldable t2, Foldable t3, CanKey k, CanKey l)
  => t1 l              -- ^ Initial state of the stack
  -> t2 (l, t3 (k, a)) -- ^ The `alist` of `alists` describing the mapping
  -> Either (LayerStackError l) (LayerStack l k a)
mkLayerStack initStack nestMaps = let
  -- Create a HashMap l (Layer k a) from the listlikes
  hms = M.fromList . map (over _2 mkLayer) $ toList nestMaps
  -- Create a HashMap (l, k) a from `hms`
  its = M.fromList $ hms ^@.. ifolded <.> (to unLayer . ifolded)
  -- Create a HashSet of keys from `its`
  kys = S.fromList . M.keys $ hms
  in case (findOf folded (not . (`S.member` kys)) initStack) of
       Nothing -> Right $ LayerStack
            { _stack = toList initStack
            , _maps  = kys
            , _items = its
            }
       Just err -> Left . LayerDoesNotExist $ err

--------------------------------------------------------------------------------
-- $ops

-- | Return a fold of all the items currently mapped to the item-key
--
-- This can be used with 'toListOf' to get an overview of all the items
-- currently mapped to an item-key, or more usefully, with 'firstOf' to simply
-- try a lookup like this: `stack^? atKey KeyA`
atKey :: (CanKey l, CanKey k)=> k -> Fold (LayerStack l k a) a
atKey c = folding $ \m -> m ^.. stack . folded . to (getK m) . folded
  where getK m n = fromMaybe [] (pure <$> M.lookup (n, c) (m^.items))

-- | Add a layer to the front of the stack and return the new 'LayerStack'. If the
-- 'Layer' does not exist, return a 'LayerStackError'.
pushLayer :: (CanKey l, CanKey k)
  => l
  -> LayerStack l k a
  -> Either (LayerStackError l) (LayerStack l k a)
pushLayer n keymap = if n `elem` keymap^.stack
  then Right $ keymap & stack %~ (n:)
  else Left  $ LayerDoesNotExist n

-- | Remove a layer from the stack. If the layer index does not exist on the
-- stack, return a 'LayerNotOnStack', if the layer index does not exist at all
-- in the 'LayerStack', return a 'LayerDoesNotExist'.
popLayer :: (CanKey l, CanKey k)
  => l
  -> LayerStack l k a
  -> Either (LayerStackError l) (LayerStack l k a)
popLayer n keymap = if
  | n `elem` keymap^.stack -> Right $ keymap & stack %~ delete n
  | n `elem` keymap^.maps  -> Left  $ LayerNotOnStack n
  | True                   -> Left  $ LayerDoesNotExist n
