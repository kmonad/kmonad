{-|
Module      : KMonad.Domain.Button.MultiTap
Description : A button that does different things depending on how often it is tapped
Copyright   : (c) David Janssen, 2019
License     : MIT

Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : non-portable (MPTC with FD, FFI to Linux-only c-code)


-}
module KMonad.Domain.Button.MultiTap
  ( mkMultiTap
  , mkMultiTapM
  )
where

import Control.Lens
import Control.Monad (void)
import Control.Monad.Trans

import KMonad.Core
import KMonad.Domain.Effect

-- | Return a button that does changes behavior based on how often it is tapped.
--
-- Each potential
mkMultiTap :: CanButton m
  => [(Microseconds, Button m)] -- ^ A list of (delay, button) tuples
  -> Button m
mkMultiTap bs = mkButton $ \case
  BPress   -> hPress bs
  BRelease -> pure ()


mkMultiTapM :: (CanButton m, Monad n)
  => [(Microseconds, Button m)] -- ^ A list of (delay, button) tuples
  -> n (Button m)
mkMultiTapM = pure . mkMultiTap


hPress :: CanButton m => [(Microseconds, Button m)] -> m ()
hPress bs = do
  hold True
  unmask <- maskInput
  nxt    <- pinComparison
  fork $ recurse nxt bs >> unmask >> hold False

recurse :: CanButton m
  => m EventComparison
  -> [(Microseconds, Button m)]
  -> m ()
recurse _   []          = pure () -- Only reachable if empty button is provided
recurse _   ((_, b):[]) = bTap b  -- Last button: press immediately
recurse nxt ((d, b):bs) = do      -- Recurse deeper when we encounter a press of the same code, or tap when delay runs out
  race (wait d) (waitForWith pred nxt) >>= \case
    Left  _ -> bTap b
    Right _ -> recurse nxt bs
  where
    pred cmp = cmp^.sameCode && cmp^._type == Press
