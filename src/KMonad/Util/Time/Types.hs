module KMonad.Util.Time.Types
  ( Time(..)
  , HasTime(..)
  , _SystemTime, _s, _ns
  , SystemTime(..)
  , Ms(..)
  )
where

import KMonad.Prelude

import RIO.Time
import Data.Time.Clock.System

--------------------------------------------------------------------------------
-- $time

-- | A point in time
newtype Time  = Time  { unTime  :: UTCTime }
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
  deriving (Eq, Show, Num, Enum, Ord, Real, Integral, Display)
makeWrapped ''Ms
