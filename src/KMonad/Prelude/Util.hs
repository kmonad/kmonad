{- For helpers that are so ubiquitous I want them everywhere -}
module KMonad.Prelude.Util
  ( fi
  , inEnv
  )
where

import KMonad.Prelude.Imports
import KMonad.Prelude.Types

-- | Shorthand for fromIntegral
fi :: (Integral a, Num b) => a -> b
fi = fromIntegral

-- | Makes some of the continuation formulations cleaner to write
inEnv :: IO m => RIO env a -> env -> m a
inEnv = flip runRIO
