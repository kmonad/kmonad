{-|
Module      : KMonad.Util
Description : Various bits and bobs that I don't know where to put
Copyright   : (c) David Janssen, 2019
License     : MIT
Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : portable

Contains code for making it slighly easier to work with time, errors, and
Acquire datatypes.

-}
module KMonad.Util
  (
    -- * Time units and utils
    -- $time
  --   Milliseconds
  -- , unMS
    -- tDiff

    -- * Random utility helpers that have no better home
    onErr
  , using
  , logRethrow

    -- * Some helpers to launch background process
  , withLaunch
  , withLaunch_
  , launch
  , launch_

  , module X
  )

where

import KMonad.Prelude

import Data.Time.Clock
import Data.Time.Clock.System

-- FIXME: Cleanup old code, then improve imports
-- import KMonad.Util.Keyboard as X
-- import KMonad.Util.FFI as K
-- import KMonad.Util.Name as X
import KMonad.Util.Time as X
import KMonad.Util.Ctx as X

--------------------------------------------------------------------------------
-- $time
--

-- | Newtype wrapper around 'Int' to add type safety to our time values
-- newtype Milliseconds = Milliseconds { unMS :: Int }
--   deriving (Eq, Ord, Num, Real, Enum, Integral, Show, Read, Generic, Display)


--------------------------------------------------------------------------------
-- $util

-- | A helper function that helps to throw errors when a return code is -1.
-- Easiest when used as infix like this:
--
-- > someFFIcall `onErr` MyCallFailedError someData
--
onErr :: (MonadUnliftIO m, Exception e) => m Int -> e -> m ()
onErr a err = a >>= \ret -> when (ret == -1) $ throwIO err

-- | Embed the action of using an 'Acquire' in a continuation monad
using :: Acquire a -> ContT r (RIO e) a
using dat = ContT $ (\next -> with dat $ \a -> next a)


-- | Log an error message and then rethrow the error
--
-- Particularly useful as a suffix using `catch`. i.e.
--
-- > doSomething `catch` logRethrow "I caught something"
logRethrow :: HasLogFunc e
  => Text
  -> SomeException -- ^ The error to throw
  -> RIO e a
logRethrow t e = do
  logError $ display t <> ": " <> display e
  throwIO e

-- | Launch a process that repeats an action indefinitely. If an error ever
-- occurs, print it and rethrow it. Ensure the process is cleaned up upon error
-- and/or shutdown.
withLaunch :: HasLogFunc e
  => Text                   -- ^ The name of this process (for logging)
  -> RIO e a                -- ^ The action to repeat forever
  -> ((Async a) -> RIO e b) -- ^ The foreground action to run
  -> RIO e b                -- ^ The resulting action
withLaunch n a f = do
  logInfo $ "Launching process: " <> display n
  withAsync
   (forever a
    `catch`   logRethrow ("Encountered error in <" <> textDisplay n <> ">")
    `finally` logInfo    ("Closing process: " <> display n))
   (\a' -> link a' >> f a')

-- | Like withLaunch, but without ever needing access to the async process
withLaunch_ :: HasLogFunc e
  => Text    -- ^ The name of this process (for logging)
  -> RIO e a -- ^ The action to repeat forever
  -> RIO e b -- ^ The foreground action to run
  -> RIO e b -- ^ The resulting action
withLaunch_ n a f = withLaunch n a (const f)

-- | Like 'withLaunch', but in the ContT monad
launch :: HasLogFunc e
  => Text    -- ^ The name of this process (for logging)
  -> RIO e a -- ^ The action to repeat forever
  -> Ctx r (RIO e) (Async a)
launch n = mkCtx . withLaunch n

-- | Like 'withLaunch_', but in the Ctx monad
launch_ :: HasLogFunc e
  => Text    -- ^ The name of this process (for logging)
  -> RIO e a -- ^ The action to repeat forever
  -> ContT r (RIO e) ()
launch_ n a = ContT $ \next -> withLaunch_ n a (next ())
