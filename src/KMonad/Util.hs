{-

All the various bits and bobs that don't fit anywhere else.

-}
module KMonad.Util
  ( -- * HasName typeclass
    -- $name
    Name
  , HasName(..)
  , Named
  , named

    -- * Time units and utils
    -- $time
  , Seconds
  , Milliseconds
  , Microseconds
  , Nanoseconds
  , Time
  , HasTime(..)
  , mkTime
  , s, ns
  , AsTime(..)
  , Timed
  , atTime

    -- * Overloaded fieldnames
    -- $thing
  , HasThing(..)

    -- * Random utility helpers that have no better home
  , pop
  )

where

import KMonad.Prelude

import Data.Time.Clock.System


--------------------------------------------------------------------------------
-- $thing
--
-- Overloaded fieldname 'thing' for things that track some property like name or
-- time alongside some 'thing'.
--

class HasThing a t | a -> t where
  thing :: Lens' a t


--------------------------------------------------------------------------------
-- $name
--
-- A way to refer to things that have a 'Name'.

type Name = Text

class HasName a where
  name :: Lens' a Name

data Named a = Named
  { _nName  :: Name
  , _nThing :: a
  } deriving (Show, Eq, Functor)
makeLenses ''Named

instance HasName (Named a)    where name = nName
instance HasThing (Named a) a where thing = nThing

named :: Name -> a -> Named a
named n = Named n

--------------------------------------------------------------------------------
-- $time
--
-- We specify some newtype wrappers around 'Int' so that we can make the
-- typechecker make sure our units line up. We also provide a 'Time' datatype
-- with some helper functions to make dealing with things that happen at a
-- certain time more elegant.

-- | Newtype wrappers around 'Int' to add typesafety to our units
newtype Seconds      = Seconds      Int
  deriving (Eq, Ord, Num, Real, Enum, Integral, Show, Read)
newtype Milliseconds = Milliseconds Int
  deriving (Eq, Ord, Num, Real, Enum, Integral, Show, Read)
newtype Microseconds = Microseconds Int
  deriving (Eq, Ord, Num, Real, Enum, Integral, Show, Read)
newtype Nanoseconds  = Nanoseconds  Int
  deriving (Eq, Ord, Num, Real, Enum, Integral, Show, Read)
makeLenses ''Seconds
makeLenses ''Milliseconds
makeLenses ''Microseconds
makeLenses ''Nanoseconds

-- | The 'Time' datatype that expresses a time value in KMonad
newtype Time = Time { unT :: SystemTime } deriving (Eq, Show, Generic)
makeLenses ''Time

-- | A 'Serialize' instance for time so we can encode and decode it easily.
instance Serialize Time where
  put = put . (systemSeconds &&& systemNanoseconds) . unT
  get = Time . uncurry MkSystemTime <$> get

-- | A classy lens style typeclass to describe "having a time value"
class HasTime a where
  time :: Lens' a Time
instance HasTime Time where
  time = id

-- | A prism to describe being able to be cast to a 'Time' value
class AsTime a where
  _Time :: Prism' a Time
instance AsTime Time where
  _Time = id

-- | A smart constructor that creates Time values from Integrals
mkTime :: (Integral a, Integral b)
  => a    -- ^ The seconds part of system time
  -> b    -- ^ The nanoseconds part of system time
  -> Time -- ^ The Time value
mkTime s' ns' = Time $ MkSystemTime (fromIntegral s') (fromIntegral ns')

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

-- | A datatype for things that happen at a certain type
data Timed a = Timed
  { _tTime :: Time
  , _tThing :: a
  } deriving (Show, Eq, Functor)
makeLenses ''Timed

instance HasTime (Timed a)    where time  = tTime
instance HasThing (Timed a) a where thing = tThing

instance Serialize a => Serialize (Timed a) where
  put v = put (v^.time) >> put (v^.thing)
  get   = (get :: Get Time) >>= \t -> atTime t <$> get

-- | A smart constructor for 'Timed' values
atTime :: Time -> a -> Timed a
atTime t = Timed t


--------------------------------------------------------------------------------
-- $util

-- I don't think this is a lawful prism
-- _Popped :: (Eq k, Hashable k)
--   => k -> Prism' (M.HashMap k v) (v, M.HashMap k v)
-- _Popped k = prism' embed match
--   where
--     embed (v, m) = m & at k .~ v
--     match =

-- | Try to look up a value in a Maplike. If it exists, return it and a new
-- Maplike without that entry. Otherwise, return Nothing and the map unchanged.
-- I wish I could do this without traversing the Maplike twice, but I haven't
-- figured out how.
pop :: At m => Index m -> m -> (Maybe (IxValue m), m)
pop idx m = case m ^. at idx of
  Just v  -> (Just v, m & sans idx)
  Nothing -> (Nothing, m)
