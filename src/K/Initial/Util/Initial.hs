-- |

module K.Initial.Util.Initial
  ( -- * Helpers
    -- $help
    whenJust
  , whenNonEmpty
  , ifM
  , duplicates
  , aflat
  , inRIO
  , reportFail

  , throwEither
  , devFail
  , ffiErr

  , fi
    -- * Reexports
  , module K.Initial.Initial
  )
where

import K.Initial.Initial

import qualified RIO.List     as L
import qualified Control.Monad.Error.Lens as Err


-- control flow ----------------------------------------------------------------

-- | Conditionally call a monadic function on 'Maybe' a value.
whenJust :: Monad m => Maybe a -> (a -> m b) -> m ()
whenJust m f = maybe (pure ()) (void . f) m

-- | Conditionally call a monadic function on a list, only when non-empty
whenNonEmpty :: Monad m => [a] -> ([a] -> m b) -> m ()
whenNonEmpty [] _ = pure ()
whenNonEmpty xs f = void . f $ xs

-- | Monadic if statement
ifM :: Monad m
  => m Bool -- ^ Monadic action yielding decision
  -> m a    -- ^ Action on True
  -> m a    -- ^ Action on False
  -> m a
ifM b x y = b >>= bool y x

-- list helpers ----------------------------------------------------------------

-- | Return a list of duplicate elements
--
-- This is slow and should not be used for time-critical tasks.
duplicates :: Eq a => [a] -> [a]
duplicates l = (L.\\) l $ L.nub l

-- | Flatten the second element in an a-list
--
-- There is probably a more general way to do that, but that is NotForNow
aflat :: [(a, [b])] -> [(a, b)]
aflat = foldMap (\(a, bs) -> map (a,) bs)



-- maybe helpers ---------------------------------------------------------------

-- | Turn a function with failure into one that also reports the mistake
--
-- When mapping some @a -> Maybe b@ over some collection, using the 'reportFail'
-- version of that function will let you easily extract the first failing value.
reportFail :: (a -> Maybe b) -> (a -> Either a b)
reportFail f a = maybe (Left a) Right $ f a

-- error helpers ---------------------------------------------------------------

-- | Either throw some error using a prism or return a pure value.
throwEither :: (MonadError e m) => AReview e t -> Either t a -> m a
throwEither l = either (errThrowing l) pure

-- | Signal programmer mistake with issue-submission instructions.
devFail :: HasCallStack => Text -> a
devFail t = error $ msg <> unpack t
  where msg = strUnlines
          [ "\nEncountered programmer error. This code should be unreachable. "
          , "Please let us know at https://github.com/kmonad/kmonad/issues "
          , "and include the following text in your submission:" ]

-- | If some monadic action returns -1, run some error action. Otherwise pure ()
--
-- Best used infix:
-- >>> ffiCall `ffiError` throwStuff
ffiErr :: (Integral i, Monad m) => m i -> m () -> m ()
ffiErr go err = go >>= \case
  (-1) -> err
  _    -> pure ()

-- context helpers -------------------------------------------------------------

inRIO :: MonadIO m => RIO env a -> env -> m a
inRIO = flip runRIO

-- shorthand -------------------------------------------------------------------

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral
