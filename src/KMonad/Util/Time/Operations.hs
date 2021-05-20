module KMonad.Util.Time.Operations

where

import KMonad.Prelude
import KMonad.Util.Time.Types

import qualified RIO.Time as T
import qualified Data.Time.Clock.System as T (systemToUTCTime)




--------------------------------------------------------------------------------
-- $manipulations

-- | Delay a time by some milliseconds
--
-- TODO: uncomment me when no-longer using delay in KMonad.Pullchain.Action
-- delay :: Ms -> Time -> Time
-- delay (Ms ms) (Time t) = Time $ T.addUTCTime d t
--   where d = fromIntegral ms / 1000

-- | Return whichever event of 2 occured first
--
-- This will require both objects to be resolved.
earliest :: (HasTime a, HasTime b) => a -> b -> Either a b
earliest a b | a^.time < b^.time = Left  a
          | otherwise         = Right b

-- | Try to do something within some time-limit, fail otherwise
--
-- TODO: uncomment me when no longer needed in KMonad.Pullchain.Action
-- within :: UIO m => Ms -> m a -> m (Maybe a)
-- within (Ms d) go = race (threadDelay $ d * 1000) go
--   >>= pure . either (const Nothing) Just

-- | Calculate how much time has elapsed between 2 time points
tDiff :: ()
  => SystemTime   -- ^ The earlier timepoint
  -> SystemTime   -- ^ The later timepoint
  -> Ms           -- ^ The time in milliseconds between the two
tDiff a b = let
  a' = T.systemToUTCTime a
  b' = T.systemToUTCTime b
  d  = T.diffUTCTime b' a'
  in round $ d * 1000
-- tDiff (MkSystemTime s_a ns_a) (MkSystemTime s_b ns_b) = let
--   s  = fromIntegral $ (s_b  - s_a) * 1000
--   ns = fromIntegral $ (ns_b - ns_a) `div` 1000000
--   in s + ns
