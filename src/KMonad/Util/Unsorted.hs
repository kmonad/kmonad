{- A home for little helper functions that don't have a better place -}
module KMonad.Util.Unsorted
  ( fi
  )
where

import KMonad.Prelude

-- | Simple short-hand for fromIntegral
fi :: (Integral a, Num b) => a -> b
fi = fromIntegral
