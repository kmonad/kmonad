{- For helpers that are so ubiquitous I want them everywhere -}
module KMonad.Prelude.Util
  ( fi
  , inEnv
  , overMVar


  , untilJust
  )
where

import KMonad.Prelude.Imports
import KMonad.Prelude.Types

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

--------------------------------------------------------------------------------
-- $maybe-flow
--
-- Easier flow-control using Maybe values

-- | Run a monadic action until a Just occurs
untilJust :: Monad m => m (Maybe a) -> m a
untilJust go = go >>= \case
  Nothing -> untilJust go
  Just a  -> pure a

