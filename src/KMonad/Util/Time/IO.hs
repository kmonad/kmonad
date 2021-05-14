module KMonad.Util.Time.IO

where

import KMonad.Prelude
import KMonad.Util.Time.Types
import KMonad.Util.Time.Operations

import qualified RIO.Time as T
import qualified Data.Time.Clock.System as T (getSystemTime)

--------------------------------------------------------------------------------
-- $io-tools

-- | Return the current time
getCurrentTime :: MIO m => m Time
getCurrentTime = Time <$> T.getCurrentTime

-- | Return the current system-time (faster than getting time and converting)
getCurrentSystemTime :: MIO m => m SystemTime
getCurrentSystemTime = liftIO T.getSystemTime

-- | Pause computation for some time
wait :: MIO m => Ms -> m ()
wait = threadDelay . (1000*) . fromIntegral

-- | Create a tick every n deciseconds
metronome :: MIO m => Ms -> m (m Time)
metronome d = pure $ wait (d * 100) >> getCurrentTime

