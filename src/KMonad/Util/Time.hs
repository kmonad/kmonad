module KMonad.Util.Time
  ( -- * Types and lenses
    Time
  , Ms
  , _SystemTime
  , _s
  , _ns

    -- * Pure operations
  , delay
  , earliest

    -- * IO operations
  , getCurrentTime
  , getCurrentSystemTime
  , wait
  , metronome
  , within

  )
where

import KMonad.Prelude hiding (wait)

import Data.Time.Clock.System

import qualified RIO.Time as T
import qualified Data.Time.Clock.System as T (getSystemTime)

--------------------------------------------------------------------------------
-- $time

-- | A point in time
newtype Time  = Time  { unTime :: T.UTCTime }
  deriving (Eq, Ord, Show)
makeWrapped ''Time

-- | The property of containing some time-value
class HasTime a where time :: Lens' a Time

instance HasTime Time where time  = id

--------------------------------------------------------------------------------
-- $system

-- | Easy conversion between Time and SystemTime
_SystemTime :: Iso' Time SystemTime
_SystemTime = _Wrapped . iso utcToSystemTime systemToUTCTime

-- | Lens into the seconds field of a 'SystemTime'
_s :: Lens' SystemTime Int64
_s = lens systemSeconds (\t s -> t { systemSeconds = s })

-- | Lens into the nanoseconds field of a 'SystemTime'
_ns :: Lens' SystemTime Word32
_ns = lens systemNanoseconds (\t ns -> t { systemNanoseconds = ns })

--------------------------------------------------------------------------------
-- $delay

-- | A vector forward in time, in ms.
newtype Ms = Ms { unMs :: Int}
  deriving (Eq, Show, Num, Enum, Ord, Real, Integral)
makeWrapped ''Ms

--------------------------------------------------------------------------------
-- $manipulations

-- | Delay a time by some milliseconds
delay :: Ms -> Time -> Time
delay (Ms ms) (Time t) = Time $ T.addUTCTime d t
  where d = fromIntegral ms / 1000

-- | Return whichever event of 2 occured first
--
-- This will require both objects to be resolved.
earliest :: (HasTime a, HasTime b) => a -> b -> Either a b
earliest a b | a^.time < b^.time = Left  a
          | otherwise         = Right b

--------------------------------------------------------------------------------
-- $io-tools

-- | Return the current time
getCurrentTime :: MonadIO m => m Time
getCurrentTime = Time <$> T.getCurrentTime

-- | Return the current system-time (faster than getting time and converting)
getCurrentSystemTime :: MonadIO m => m SystemTime
getCurrentSystemTime = liftIO T.getSystemTime

-- | Pause computation for some time
wait :: MonadIO m => Ms -> m ()
wait = threadDelay . (1000*) . fromIntegral

-- | Create a tick every n deciseconds
metronome :: MonadIO m => Ms -> m (m Time)
metronome d = pure $ wait (d * 100) >> getCurrentTime

-- | Try to do something within some time-limit, fail otherwise
within :: MonadUnliftIO m => Ms -> m a -> m (Maybe a)
within (Ms d) go = race (threadDelay $ d * 1000) go
  >>= pure . either (const Nothing) Just
