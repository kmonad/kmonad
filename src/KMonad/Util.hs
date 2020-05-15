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
  , tDiff

    -- * Overloaded fieldnames
    -- $fields
  , HasThing(..)

    -- * Random utility helpers that have no better home
  , pop
  , onErr
  , withReader
  , withLaunch
  , launch
  , withLaunch_
  , launch_
  , using
  , logRethrow
  )

where

import KPrelude

import Data.Time.Clock.System



--------------------------------------------------------------------------------
-- $fields
--
-- Overloaded fieldname 'thing' for things that track some property like name or
-- time alongside some 'thing'.
--

class HasThing a t | a -> t where
  thing :: Lens' a t

-- class HasCfg a t | a -> t where
--   cfg :: Lens' a t

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
  deriving (Eq, Ord, Num, Real, Enum, Integral, Show, Read, Generic, Display)
newtype Milliseconds = Milliseconds Int
  deriving (Eq, Ord, Num, Real, Enum, Integral, Show, Read, Generic, Display)
newtype Microseconds = Microseconds Int
  deriving (Eq, Ord, Num, Real, Enum, Integral, Show, Read, Generic, Display)
newtype Nanoseconds  = Nanoseconds  Int
  deriving (Eq, Ord, Num, Real, Enum, Integral, Show, Read, Generic, Display)

_s :: Lens' SystemTime Seconds
_s = let
  get (MkSystemTime s _)    = fromIntegral s
  set (MkSystemTime _ ns) s = MkSystemTime (fromIntegral s) ns
  in lens get set

_ns :: Lens' SystemTime Nanoseconds
_ns = let
  get (MkSystemTime _ ns)    = fromIntegral ns
  set (MkSystemTime s _)  ns = MkSystemTime s (fromIntegral ns)
  in lens get set

tDiff :: SystemTime -> SystemTime -> Milliseconds
tDiff a b = let
  s  = fromIntegral $ (b^._s - a^._s) * 1000
  ns = fromIntegral $ (b^._ns - a^._ns) `div` 1000000
  in s + ns

-- | Return how many milliseconds have elapsed since 'Time'
-- since :: MonadIO m => Time -> m Milliseconds
-- since t = now $ \n -> t `tDiff` n

-- | Wrap a thing in a 'Timed' wrapper with the current time
-- stampNow :: MonadIO m => a -> m (Timed a)
-- stampNow a = now $ flip atTime a

-- --------------------------------------------------------------------------------
-- -- $pprint

-- instance Display Time where
--   textDisplay t
--     = tshow (fromIntegral $ t^._s :: Int)
--       <> "."
--       <> tshow (fromIntegral $ t^._ns :: Int)


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

-- | A reimplementation of 'withReader' from MTL on top of RIO. It is like
-- 'local' but changes the type of the environment.
withReader :: (e' -> e) -> RIO e a -> RIO e' a
withReader f a = ask >>= \env -> runRIO (f env) a

-- | Embed the action of using an 'Acquire' in a continuation monad
using :: Acquire a -> ContT r (RIO e) a
using dat = ContT $ (\next -> with dat $ \a -> next a)

-- | Launch a thread that repeats an action indefinitely. If an error ever
-- occurs, print it and rethrow it. Ensure the thread is cleaned up upon error
-- and/or shutdown.
withLaunch :: HasLogFunc e
  => Name                   -- ^ The name of this thread (for logging purposes only)
  -> RIO e a                -- ^ The action to repeat forever in the background
  -> ((Async a) -> RIO e b) -- ^ The foreground action to run
  -> RIO e b                -- ^ The resulting action
withLaunch n a f = do
  logInfo $ "Launching thread: " <> display n
  withAsync
   (forever a
    `catch`   logRethrow ("Encountered error in <" <> display n <> ">")
    `finally` logInfo    ("Closing thread: " <> display n))
   (\a' -> link a' >> f a')

withLaunch_ :: HasLogFunc e
  => Name    -- ^ The name of this thread (for logging purposes only)
  -> RIO e a -- ^ The action to repeat forever in the background
  -> RIO e b -- ^ The foreground action to run
  -> RIO e b -- ^ The resulting action
withLaunch_ n a f = withLaunch n a (const f)

launch :: HasLogFunc e
  => Name
  -> RIO e a
  -> ContT r (RIO e) (Async a)
launch n = ContT . withLaunch n

launch_ :: HasLogFunc e
  => Name
  -> RIO e a
  -> ContT r (RIO e) ()
launch_ n a = ContT $ \next -> withLaunch_ n a (next ())


logRethrow :: HasLogFunc e => Utf8Builder -> SomeException -> RIO e a
logRethrow t e = do
  logError $ t <> ": " <> display e
  throwIO e
