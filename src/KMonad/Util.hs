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
  , _SystemTime
  , mkTime
  , Timed
  , atTime
  , now
  , stampNow

    -- * Overloaded fieldnames
    -- $fields
  , HasThing(..)
  , HasCfg(..)

    -- * Support for pretty-printing
    -- $pprint
  , PrettyPrint(..)

    -- * Random utility helpers that have no better home
  , pop
  , onErr
  )

where

import Prelude

import Data.Time.Clock.System

import qualified RIO.HashMap as M
import qualified RIO.HashSet as S


--------------------------------------------------------------------------------
-- $fields
--
-- Overloaded fieldname 'thing' for things that track some property like name or
-- time alongside some 'thing'.
--

class HasThing a t | a -> t where
  thing :: Lens' a t

class HasCfg a t | a -> t where
  cfg :: Lens' a t

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
  deriving (Eq, Ord, Num, Real, Enum, Integral, Show, Read, Generic)
newtype Milliseconds = Milliseconds Int
  deriving (Eq, Ord, Num, Real, Enum, Integral, Show, Read, Generic)
newtype Microseconds = Microseconds Int
  deriving (Eq, Ord, Num, Real, Enum, Integral, Show, Read, Generic)
newtype Nanoseconds  = Nanoseconds  Int
  deriving (Eq, Ord, Num, Real, Enum, Integral, Show, Read, Generic)

instance Serialize Seconds
instance Serialize Nanoseconds


-- | The 'Time' datatype that expresses a time value in KMonad
data Time = Time
  { __s  :: Seconds
  , __ns :: Nanoseconds
  } deriving (Eq, Show, Generic)
makeClassy ''Time


-- | A 'Serialize' instance for time so we can encode and decode it easily.
instance Serialize Time where
  put = put . (__s &&& __ns)
  get = uncurry Time <$> get

-- | An Iso to map between Time and SystemTime values easily
_SystemTime :: Iso' Time SystemTime
_SystemTime = iso to' from'
  where
    to'   = uncurry MkSystemTime .
      (fromIntegral . __s &&& fromIntegral . __ns)
    from' = uncurry Time .
      (fromIntegral . systemSeconds &&& fromIntegral . systemNanoseconds)

  -- where convert = uncurry Time . (fromIntegral . systemSeconds &&& fromIntegral . systemNanoseconds)

-- | A smart constructor that creates Time values from Integrals
mkTime :: (Integral a, Integral b)
  => a    -- ^ The seconds part of system time
  -> b    -- ^ The nanoseconds part of system time
  -> Time -- ^ The Time value
mkTime s ns = Time (fromIntegral s) (fromIntegral ns)

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

instance PrettyPrint a => PrettyPrint (Timed a) where
  pprint a = pprint (a^.time) <> ": " <> pprint (a^.thing)

-- | A smart constructor for 'Timed' values
atTime :: Time -> a -> Timed a
atTime t = Timed t

-- | Run a computation that requires a time now
now :: MonadIO m => (Time -> a) -> m a
now f = f . (view $ from _SystemTime) <$> liftIO getSystemTime

-- | Wrap a thing in a 'Timed' wrapper with the current time
stampNow :: MonadIO m => a -> m (Timed a)
stampNow a = now $ flip atTime a

--------------------------------------------------------------------------------
-- $pprint

class PrettyPrint a where
  pprint     :: a -> Text
  pprintDisp :: a -> Utf8Builder
  pprintDisp = display . pprint

instance PrettyPrint Time where
  pprint t = tshow (fromIntegral $ t^._s :: Int)
          <> "."
          <> tshow (fromIntegral $ t^._ns :: Int)

--------------------------------------------------------------------------------
-- $util

-- | Try to look up a value in a Maplike. If it exists, return it and a new
-- Maplike without that entry. Otherwise, return Nothing and the map unchanged.
-- I wish I could do this without traversing the Maplike twice, but I haven't
-- figured out how.
pop :: At m => Index m -> m -> (Maybe (IxValue m), m)
pop idx m = case m ^. at idx of
  Just v  -> (Just v, m & sans idx)
  Nothing -> (Nothing, m)

-- | A helper function that helps to throw errors when a return code is -1.
-- Easiest when used as infix like this:
--
-- >>> someFFIcall `onErr` MyCallFailedError someData
--
onErr :: (MonadUnliftIO m, Exception e) => m Int -> e -> m ()
onErr a err = a >>= \ret -> when (ret == -1) $ throwIO err

