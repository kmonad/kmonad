{-|
Module      : KMonad.Domain.Effect
Description : Collection of various effects used in KMonad
Copyright   : (c) David Janssen, 2019
License     : MIT

Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : non-portable (MPTC with FD, FFI to Linux-only c-code)

This module started as an attempt to keep IO out of the actual 'Button'
implementations. Allowing IO in 'Button's makes it impossible to reason about
future 'Button's introduced by third parties.

Then there was the choice of making 1 large /MonadButton/ Monad, or many little
ones. There are many different points one can choose in the design space, and I
might've erred on the side of granularity.

Any concrete API must implement all of the effects for KMonad to typecheck. Some
Monad's are simply ways of plugging utility into the Domain (like
'MonadHandler': which only passes the handling function into Domain code). Other
effects are meant to be included by 'Button' definitions, to allow them access
to a variety of side-effects.

-}
module KMonad.Domain.Effect
  ( -- * Collections of effects
    CanButton
  , CanButtonIO
  , CanApi

    -- * MonadEmit
    -- $emit
  , MonadEmit(..)
  , emitSeq, emitPress, emitRelease

    -- * MonadFork
    -- $fork
  , MonadFork(..)

    -- * MonadFuture
    -- $future
  , MonadFuture(..)
  , waitFor
  , waitForWith
  , waitForM
  , waitForWithM

    -- * MonadHandler
    -- $handler
  , MonadHandler(..)

    -- * MonadHold
    -- $hold
  , MonadHold(..)
  , holdDuring

    -- * MonadInject
    -- $inject
  , MonadInject(..)

    -- * MonadLock
    -- $lock
  , MonadLock(..)

    -- * MonadMaskInput
    -- $maskinput
  , MonadMaskInput(..)

    -- * MonadNext
    -- $next
  , MonadNext(..)

    -- * MonadNow
    -- $now
  , MonadNow(..)
  , withNow
  , nowIO

    -- * MonadRace
    -- $race
  , MonadRace(..)

    -- * MonadStackManip
    -- $stack
  , MonadStackManip(..)

    -- * MonadSymbol
    -- $symbol
  , MonadSymbol(..)

    -- * MonadTrace
    -- $trace
  , MonadTrace(..)
  , showTrace

    -- * MonadVar
    -- $var
  , MonadVar(..)
  , Var
  , runVar, swapVar, getV, putV, readVar

    -- * MonadWait
    -- $wait
  , MonadWait(..)

    -- * Reexports
  , module Control.Monad.Logger
  )
where

import Control.Concurrent.MVar
import Control.Lens
import Control.Monad.Except
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.Writer
import Data.Text
import Data.Time.Clock.System (getSystemTime)

import qualified Data.Text    as T
import qualified Data.Text.IO as T

import KMonad.Core

--------------------------------------------------------------------------------
-- $collection
--
-- We define a number of different collections of effects that describe the
-- supported contexts for different computations.

-- | The collection of effects supported by standard 'Button's. Note that, if
-- you are extending KMonad by adding a new 'Button', it is clearer if you
-- explicitly state which effects you are including in the type-signature. For
-- 'Button's that wrap other 'Button's (like TapHold or Around), you will need
-- to include all effects.
type CanButton m =
  ( MonadEmit       m
  , MonadFork       m
  , MonadFuture     m
  , MonadHold       m
  , MonadInject     m
  , MonadLock       m
  , MonadLogger     m
  , MonadMaskInput  m
  , MonadNext       m
  , MonadNow        m
  , MonadRace       m
  , MonadStackManip m
  , MonadSymbol     m
  , MonadTrace      m
  , MonadVar        m
  , MonadWait       m
  )

-- | The collection of effects if we want to support arbitrary IO 'Button's
--
-- TODO: implement an /ArbitraryIO/ style 'Button' and a secondary
-- implementation of handle that supports IO buttons so that people can, if they
-- really want to, include arbitrary actions bound to keys. This will be
-- something we will only support in the Haskell interface. The interpreted
-- config-files shouldn't even consider trying to write a parser/interpreter for
-- arbitrary effects.
type CanButtonIO m =
  ( CanButton m
  , MonadIO m
  )

-- | The collection of effects that must be supported by any API
type CanApi m =
  ( CanButton m
  , MonadIO m
  , MonadHandler m
  )

--------------------------------------------------------------------------------
-- $emit

-- | The 'MonadEmit' effect allows sending KeyEvent's to the OS.
class MonadNow m => MonadEmit m where
  emitKey :: KeyAction -> m ()

emitSeq :: (MonadEmit m) => KeySequence -> m ()
emitSeq = mapM_ emitKey

emitPress :: MonadEmit m => KeyCode -> m ()
emitPress = emitKey . mkKeyPress

emitRelease :: MonadEmit m => KeyCode -> m ()
emitRelease = emitKey . mkKeyRelease

--------------------------------------------------------------------------------
-- $fork

-- | The 'MonadFork' effect allows forking operations
class Monad m => MonadFork m where
  fork :: m () -> m ()


--------------------------------------------------------------------------------
-- $future

-- | FIXME: documentation
class Monad m => MonadFuture m where
  waitNext :: m (m KeyEvent)

-- | Given a predicate on EventComparisons, return () once the predicate is
-- satisfied.
--
-- This is often used in conjunction with 'race' to do something in the case
-- some condition is met within a given time.
-- For example:
--
-- >>> race (wait 1000000) (waitFor (\c -> (c^.eventType == Release)) >> doThing)
--
waitFor :: MonadFuture m => (KeyEvent-> Bool) -> m ()
waitFor p = waitNext >>= waitForWith p

waitForM :: MonadFuture m => (KeyEvent-> m Bool) -> m ()
waitForM p = waitNext >>= waitForWithM p

-- | Like waitFor, but using an already pinned action
--
-- `waitFor p` is equivalent to `pinComparison >>= waitForWith p`
waitForWith :: Monad m
  => (a -> Bool) -- ^ The predicate to match
  -> m a         -- ^ The action that generates comparisons
  -> m ()
waitForWith p nxt = nxt >>= \cmp -> if p cmp then pure () else waitForWith p nxt

waitForWithM :: Monad m
  => (a -> m Bool) -- ^ The effectful operation to use for matching
  -> m a           -- ^ The action that generates comparisons
  -> m ()
waitForWithM p nxt = nxt >>= \cmp -> p cmp >>= \case
  True  -> pure ()
  False -> waitForWithM p nxt


--------------------------------------------------------------------------------
-- $handler

-- | This typeclass is simply used to inject a 'KeyEvent' handling function into
-- domain-code. Any concrete API needs to implement some method of dealing with
-- 'KeyEvent's and plug it into Domain code by defining an instance of this
-- class.
class Monad m => MonadHandler m where
  handle :: KeyEvent -> m ()


--------------------------------------------------------------------------------
-- $hold

-- | This effect allows the pausing of handling button events. For example, a
-- 'KMonad.Core.Parser.Token.BTapHold' pauses processing until it decides what
-- action to take, and then resumes processing again.
class Monad m => MonadHold m where
  hold :: Bool -> m ()

-- | Put a hold on processing while performing an action.
holdDuring :: MonadHold m => m a -> m a
holdDuring a = do
  hold True
  x <- a
  hold False
  return x


--------------------------------------------------------------------------------
-- $inject


-- | This effect allows the injection of internally generated 'Event's that will
-- be pulled by 'nextEvent'. This is the mechanism by which we can cleanly
-- shutdown KMonad (by passing a 'KMonad.Core.Event.Quit' event).
class Monad m => MonadInject m where
  injectEvent :: Event -> m ()


--------------------------------------------------------------------------------
-- $lock

-- | This allows for the toggling of keyboard locking keys while keeping track
-- of the state internally.
class Monad m => MonadLock m where
  lockOn     :: LockKey -> m ()
  lockOff    :: LockKey -> m ()
  lockToggle :: LockKey -> m ()


--------------------------------------------------------------------------------
-- $maskinput

-- | This effect allows the masking of normal handling of the current event.
-- This is necessary for mapping multiple presses of the same button to 1 output
-- event.
class Monad m => MonadMaskInput m where
  -- | Mask events with the same keycode as the current event from being
  -- processed, and return the action to unmask processing again.
  maskInput :: m (m ())

--------------------------------------------------------------------------------
-- $next

-- | An API must implement how to perform a blocking call awaiting the next
-- event to occur.
class Monad m => MonadNext m where
  nextEvent :: m Event


--------------------------------------------------------------------------------
-- $now

-- | This effect allows access to the current time.
class Monad m => MonadNow m where
  now :: m Time

instance MonadNow IO where
  now = nowIO

-- | Get the current time using IO
nowIO :: MonadIO m => m Time
nowIO = view (from isoSystemTime) <$> liftIO getSystemTime

-- | A 'withNow' helper function to generate MonadNow calculations
withNow :: MonadNow m => (Time -> a) -> m a
withNow f = f <$> now


--------------------------------------------------------------------------------
-- $race

-- | This effect allows for racing two different actions to see which finishes
-- first. The moment 1 action finishes, the other is cancelled.
class Monad m => MonadRace m where
  race :: m a -> m b -> m (Either a b)


--------------------------------------------------------------------------------
-- $stack

-- | This effect allows for the manipulation of the stack of layers.
class Monad m => MonadStackManip m where
  pushL :: Name -> m ()
  popL  :: Name -> m ()


--------------------------------------------------------------------------------
-- $symbol

-- | The 'MonadSymbol' effect, used to emit special characters using
-- key-sequences.

class  MonadEmit m => MonadSymbol m where
  emitSymbol  :: SpecialSymbol -> m ()
  emitDeadKey :: DeadKey -> m ()


--------------------------------------------------------------------------------
-- $trace

-- | This effect simply allows text to be written to stdout
class Monad m => MonadTrace m where
  trace :: Text -> m ()

-- | Output the 'show' of a value
showTrace :: (MonadTrace m, Show a) => a -> m ()
showTrace = trace . T.pack . show


instance MonadTrace IO where
  trace t = liftIO $ T.putStrLn t >> return mempty
instance Monad m => MonadTrace (WriterT [Text] m) where
  trace = tell . (:[])


--------------------------------------------------------------------------------
-- $var
--
-- This effect allows maintaining a piece of thread-safe, local state. This is
-- necessary for certain button operations that need to keep track of what state
-- they exist in. This is a very shallow abstraction over MVar, so note that
-- getting and putting values can block.

-- | A reference to some piece of local state
newtype Var a = Var { unVar :: MVar a }

-- | Create an action that uses a 'Var' to manage local state
runVar :: (MonadIO m) => a -> (Var a -> b) -> m b
runVar a f = do
  v <- liftIO . newMVar $ a
  return $ f (Var v)

-- | Simple MonadIO implementation of getVar
getV :: MonadIO m => Var a -> m a
getV v = liftIO $ takeMVar $ unVar v

-- | Simple MonadIO implementation of putVar
putV :: MonadIO m => a -> Var a -> m ()
putV a v = liftIO $ putMVar (unVar v) a

-- | The MonadVar class that allows the creation and manipulation of local state
class Monad m => MonadVar m where
  getVar :: Var a -> m a
  putVar :: a -> Var a -> m ()

-- | Swap the value in a Var and return the old one
swapVar :: MonadVar m => a -> Var a -> m a
swapVar a v = getVar v >>= \old -> putVar a v >> return old

-- | Read the value from a Var without taking it
readVar :: MonadVar m => Var a -> m a
readVar v = getVar v >>= \x -> putVar x v >> pure x


--------------------------------------------------------------------------------
-- $wait

-- | Allow an action to wait for a bit.
class Monad m => MonadWait m where
  wait :: Microseconds -> m ()



-- $compound

-- emitSeq :: (MonadNow m, MonadEmit m) => KeySequence -> m ()
-- emitSeq ks = mapM_ emit . withNow $ runSequence ks
