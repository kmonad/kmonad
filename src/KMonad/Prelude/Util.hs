{- For helpers that are so ubiquitous I want them everywhere -}
module KMonad.Prelude.Util
  ( fi
  , inEnv
  , overMVar
  , reverseMap
  , duplicates

  , untilJust
  )
where

import KMonad.Prelude.Imports
import KMonad.Prelude.Types

import qualified RIO.HashMap as M
import qualified RIO.Set     as S

--------------------------------------------------------------------------------
-- $uncategorized

-- | Shorthand for fromIntegral
fi :: (Integral a, Num b) => a -> b
fi = fromIntegral

-- | Makes some of the continuation formulations cleaner to write
inEnv :: IO m => RIO env a -> env -> m a
inEnv = flip runRIO

-- | Slightly different modifyMVar to make 1-liners cleaner
overMVar :: UIO m =>  m (MVar a) -> (a -> m (a, b))  -> m b
overMVar a f = a >>= \mv -> modifyMVar mv f

-- | Reverse a map
--
-- NOTE: This might overwrite duplicates.
reverseMap :: (Eq a, Eq b, Hashable a, Hashable b)
  => M.HashMap a b -> M.HashMap b a
reverseMap = M.fromList . toListOf (folded . swapped) . M.toList

-- | Return a set of entries that occur at least more than once
duplicates :: (Foldable t, Ord a) => t a -> S.Set a
duplicates as = snd $ foldl' go (S.empty, S.empty) as where
  go (seen, res) a | a `S.member` seen = (seen, S.insert a res)
                   | otherwise         = (S.insert a seen, res)

--------------------------------------------------------------------------------
-- $maybe-flow
--
-- Easier flow-control using Maybe values

-- | Run a monadic action until a Just occurs
untilJust :: Monad m => m (Maybe a) -> m a
untilJust go = go >>= \case
  Nothing -> untilJust go
  Just a  -> pure a

