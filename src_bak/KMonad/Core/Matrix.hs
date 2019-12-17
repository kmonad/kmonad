{-# LANGUAGE DeriveAnyClass #-}
{-|
Module      : KMonad.Core.Matrix
Description : Simple implementation of sparse matrices
Copyright   : (c) David Janssen, 2019
License     : MIT
Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : portable

A very simple and unsophisticated implementation of sparse 2-d matrices.
-}
module KMonad.Core.Matrix
  ( -- * The Coor type, constructor, and optics
    -- $coor
    Coor
  , mkCoor
  , coorTup
  , _x, _y

    -- * The Matrix type, constructor, and optics
    -- $matrix
  , Matrix
  , matList
  , matMap
  , items
  , coors

    -- * Operations on Coor and Matrix data
    -- $util
  , minCoor
  , negCoor
  , shift
  , findCoorFor
  , normalize
  , overlay
  , anchorTo
  )
where

import Control.Lens
import Data.Foldable
import Data.Hashable
import Data.List (find)
import GHC.Generics (Generic)

import qualified Data.HashMap.Strict as M

--------------------------------------------------------------------------------
-- $coor
--
-- 'Coor' describes the index into a 2D 'Matrix'.
--
-- 'Coor's form a Monoid under pairwise addition and the origin. Other monoids
-- are possible, but irrelevant for our purposes.

-- | The 2D coordinate indexing into the sparse matrix.
newtype Coor = Coor { unCoor :: (Int, Int) }
  deriving stock    (Eq, Generic, Ord, Show)
  deriving anyclass (Hashable)
makeWrapped ''Coor

instance Semigroup Coor where
  a <> b = mkCoor (a^._x + b^._x) (a^._y + b^._y)
instance Monoid Coor where
  mempty = mkCoor 0 0

-- | A constructor for coordinates
mkCoor :: Int -> Int -> Coor
mkCoor x y = Coor (x, y)

-- | An Iso between coordinates and tuples of Int
--
-- TODO: replace this with Wrapped
coorTup :: Iso' Coor (Int, Int)
coorTup = iso unCoor Coor

-- | A lens into the x field of a coordinate
_x :: Lens' Coor Int
_x = coorTup._1

-- | A lens into the y field of a coordinate
_y :: Lens' Coor Int
_y = coorTup._2

-- | Negate a coordinate
negCoor :: Coor -> Coor
negCoor c = c & (coorTup . both) *~ (-1)


--------------------------------------------------------------------------------
-- $matrix

-- | A sparse collection of indexed data
newtype Matrix a = Matrix { unMatrix :: M.HashMap Coor a }
  deriving stock    (Eq, Show, Functor, Foldable)
  deriving anyclass (Ixed)

-- | Instances for some lens classes
type instance IxValue (Matrix a) = a
type instance Index   (Matrix a) = Coor

instance At (Matrix a) where
  at c = matMap.(at c)

-- | An Iso between a Matrix and a HashMap of coordinates
--
-- TODO: Replace this with Wrapped
matMap :: Iso' (Matrix a) (M.HashMap Coor a)
matMap = iso unMatrix Matrix

-- | An Iso between a Matrix and an Alist of coordinates and values
matList :: Iso' (Matrix a) [(Coor, a)]
matList = iso (M.toList . unMatrix) (Matrix . M.fromList)

-- | A Getter to get the items out of a Matrix
items :: Getter (Matrix a) [a]
items = to toList

-- | A Getter to get the coors out of a Matrix
coors :: Getter (Matrix a) [Coor]
coors = to $ \m -> map fst $ m^.matList

--------------------------------------------------------------------------------
-- $util

-- | Return the 'Coor' containing the lowest x and y coordinate. Note that this
-- 'Coor' does not necessarily correspond to an item that exist in the Matrix
-- itself, as 1 item might have the lowest x, and another the lowest y value.
minCoor :: Matrix a -> Coor
minCoor m = mkCoor minX minY
  where minX = minimum . map (view _x) $ m^.coors
        minY = minimum . map (view _y) $ m^.coors

-- | Shift all the coordinates by mappending them with the provided 'Coor'
shift :: Coor -> Matrix a -> Matrix a
shift c = matList . mapped . _1 <>~ c

-- | Ensure that the 'minCoor' is (0, 0)
normalize :: Matrix a -> Matrix a
normalize m = shift (negCoor $ minCoor m) m

-- | Find the 'Coor' for a particular item, or Nothing if item doesn't exist
findCoorFor :: Eq a => a -> Matrix a -> Maybe Coor
findCoorFor x m = fst <$> find ((== x) . snd) (m^.matList)

-- | Shift a matrix to align with an element from a source matrix If the source
-- matrix does not contain the provided element return Nothing. If combined with
-- 'normalize' this ensures that the 'minCoor' of the second 'Matrix' lies
-- exactly on the 'Coor' of the item in the first 'Matrix'.
anchorTo :: Eq a => a -> Matrix a -> Matrix b -> Maybe (Matrix b)
anchorTo x a b = flip shift b <$> findCoorFor x a

-- | Coregister two matrices: For every element in b find the element at the
-- corresponding coordinate in a. Every element in b must have an element at
-- the same coordinate in a, but the reverse need not be true. If an element
-- is not found in the source layer, that coordinate is returned in a Left
overlay :: Matrix a -> Matrix b -> Either Coor [(a, b)]
overlay a b = foldlM f [] (b^.matList)
  where f acc (c, ib) = case a^.at c of
          Just x  -> pure $ (x, ib):acc
          Nothing -> Left c
