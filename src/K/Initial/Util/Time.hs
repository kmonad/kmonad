{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- |

module K.Initial.Util.Time
  ( -- * Length of time
    -- $dt
    Dt
  , us
  , ms

    -- * System time
  , Time
  , now
  , tS
  , tNS
  )

where

import K.Initial.Util.Initial

import Data.Time.Clock.System

-- length of time --------------------------------------------------------------

-- | A duration of time encoded as some non-negative amount of microseconds
newtype Dt = Dt { _us :: Natural }
  deriving (Num, Eq, Ord, Show, Read, Generic)
makeLenses ''Dt

-- | A lens between a non-negative amount of milliseconds and 'Dt'
ms :: Iso' Dt Natural
ms = iso (view $ us . to (`div` 1000)) (Dt . (* 1000))

-- point in time ---------------------------------------------------------------

newtype Time = Time { _time :: SystemTime }

tS :: Integral a => Getter Time a
tS = to $ \(Time st) -> fi . systemSeconds $ st

tNS :: Integral a => Getter Time a
tNS = to $ \(Time st) -> fi . systemNanoseconds $ st

now :: MonadIO m => m Time
now = Time <$> liftIO getSystemTime
