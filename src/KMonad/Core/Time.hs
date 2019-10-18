{-|
Module      : KMonad.Core.Time
Description : How KMonad handles Time values
Copyright   : (c) David Janssen, 2019
License     : MIT
Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : portable
-}
module KMonad.Core.Time
  ( -- * Types and lenses
    -- $types
    Seconds, Milliseconds, Microseconds, Nanoseconds
  , Time
  , HasTime(..)
  , mkTime
  , s, ns
  , isoSystemTime

    -- * Operations on Time values
    -- $util
  , tminus
  )
where

import           Control.Lens
import           Data.Time.Clock.System

--------------------------------------------------------------------------------
-- $types

-- | Newtype wrappers to get the type-checkers assurances of using the right units
newtype Seconds      = Seconds      Int deriving (Eq, Ord, Num, Real, Enum, Integral, Show, Read)
newtype Milliseconds = Milliseconds Int deriving (Eq, Ord, Num, Real, Enum, Integral, Show, Read)
newtype Microseconds = Microseconds Int deriving (Eq, Ord, Num, Real, Enum, Integral, Show, Read)
newtype Nanoseconds  = Nanoseconds  Int deriving (Eq, Ord, Num, Real, Enum, Integral, Show, Read)

-- | The 'Time' datatype that expresses a time value in KMonad
newtype Time = Time { unT :: SystemTime } deriving (Eq, Show)

-- | A classy lens style typeclass to describe "having a time value"
class HasTime a where
  time :: Lens' a Time
instance HasTime Time where
  time = id

-- | A smart constructor that creates Time values from Integrals
mkTime :: (Integral a, Integral b)
  => a    -- ^ The seconds part of system time
  -> b    -- ^ The nanoseconds part of system time
  -> Time -- ^ The Time value
mkTime s' ns' = Time $ MkSystemTime (fromIntegral s') (fromIntegral ns')

-- | An Iso between 'Time' and 'SystemTime'
isoSystemTime :: Iso' Time SystemTime
isoSystemTime = iso unT Time

-- | A lens into the seconds field of a 'Time' value
s :: Lens' Time Seconds
s = lens getter setter
  where getter = fromIntegral . systemSeconds . unT
        setter old s' = mkTime s' (old^.ns)

-- | A lens into the nanoseconds field of a 'Time' value
ns :: Lens' Time Nanoseconds
ns = lens getter setter
  where getter = fromIntegral . systemNanoseconds . unT
        setter old ns' = mkTime (old^.s) ns'


--------------------------------------------------------------------------------
-- $util

-- | Get the time elapsed in Nanoseconds between the first and second time
tminus :: Time -> Time -> Nanoseconds
tminus a b = let ds  = (a^.s)  - (b^.s)
                 dns = (a^.ns) - (b^.ns) in
               -(fromIntegral (10^(9::Int) * ds) + dns)

