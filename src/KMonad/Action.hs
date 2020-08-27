{-|
Module      : KMonad.Action
Description : Collection of basic operations
Copyright   : (c) David Janssen, 2019
License     : MIT
Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : portable

KMonad is implemented as an engine that is capable of running 'MonadK' actions.
The logic of various different buttons and keyboard operations are expressed in
this 'MonadK'. This module defines the basic types and operations that make up
'MonadK'. The implementation of how KMonad implements 'MonadK' can be found in
the "KMonad.App" module.

-}
module KMonad.Action
  (
    KeyPred
  , Catch(..)
  , Trigger(..)
  , Timeout(..)
  , HookLocation(..)
  , Hook(..)

    -- * Lenses
  , HasHook(..)
  , HasTimeout(..)
  , HasTrigger(..)

    -- * Layer operations
    -- $lop
  , LayerOp(..)

    -- * MonadK
    -- $monadk
  , MonadKIO(..)
  , MonadK(..)
  , AnyK
  , Action(..)

    -- * Constituted actions
    -- $combs
  , my
  , matchMy
  , after
  , whenDone
  , await
  , awaitMy
  , tHookF
  , hookF
  , within
  , withinHeld
  )

where

import KMonad.Prelude hiding (timeout)

import KMonad.Keyboard
import KMonad.Util

--------------------------------------------------------------------------------
-- $keyfun

-- | Boolean isomorph signalling wether an event should be caught or not
data Catch = Catch | NoCatch deriving (Show, Eq)

instance Semigroup Catch where
  NoCatch <> NoCatch = NoCatch
  _       <> _       = Catch

instance Monoid Catch where
  mempty = NoCatch

-- | The packet used to trigger a KeyFun, containing info about the event and
-- how long since the Hook was registered.
data Trigger = Trigger
  { _elapsed :: Milliseconds -- ^ Time elapsed since hook was registered
  , _event   :: KeyEvent     -- ^ The key event triggering this call
  }
makeClassy ''Trigger


--------------------------------------------------------------------------------
-- $hook
--
-- The general structure of the 'Hook' record, that defines the most general way
-- of registering a 'KeyEvent' function.

-- | ADT signalling where to install a hook
data HookLocation
  = InputHook  -- ^ Install the hook immediately after receiving a 'KeyEvent'
  | OutputHook -- ^ Install the hook just before emitting a 'KeyEvent'
  deriving (Eq, Show)

-- | A 'Timeout' value describes how long to wait and what to do upon timeout
data Timeout m = Timeout
  { _delay  :: Milliseconds -- ^ Delay before timeout action is triggered
  , _action :: m ()         -- ^ Action to perform upon timeout
  }
makeClassy ''Timeout

-- | The content for 1 key hook
data Hook m = Hook
  { _hTimeout :: Maybe (Timeout m)  -- ^ Optional timeout machinery
  , _keyH     :: Trigger -> m Catch -- ^ The function to call on the next 'KeyEvent'
  }
makeClassy ''Hook


--------------------------------------------------------------------------------
-- $lop
--
-- Operations that manipulate the layer-stack

-- | 'LayerOp' describes all the different layer-manipulations that KMonad
-- supports.
data LayerOp
  = PushLayer    LayerTag -- ^ Add a layer to the top of the stack
  | PopLayer     LayerTag -- ^ Remove the first occurence of a layer
  | SetBaseLayer LayerTag -- ^ Change the base-layer


--------------------------------------------------------------------------------
-- $monadk
--
-- The fundamental components that make up any 'KMonad.Button.Button' operation.

-- | 'MonadK' contains all the operations used to constitute button actions. It
-- encapsulates all the side-effects required to get everything running.
class Monad m => MonadKIO m where
  -- | Emit a KeyEvent to the OS
  emit       :: KeyEvent -> m ()
  -- | Pause the current thread for n milliseconds
  pause      :: Milliseconds -> m ()
  -- | Pause or unpause event processing
  hold       :: Bool -> m ()
  -- | Register a callback hook
  register   :: HookLocation -> Hook m -> m ()
  -- | Run a layer-stack manipulation
  layerOp    :: LayerOp -> m ()

class MonadKIO m => MonadK m where
  -- | Access the keycode to which the current button is bound
  myBinding  :: m Keycode

-- | Type alias for `any monad that can perform MonadK actions`
type AnyK a = forall m. MonadK m => m a

-- | A newtype wrapper used to construct 'MonadK' actions
newtype Action = Action { runAction :: AnyK ()}

--------------------------------------------------------------------------------
-- $util

-- | Create a KeyEvent matching pressing or releasing of the current button.
my :: MonadK m => Switch -> m KeyEvent
my s = mkKeyEvent s <$> myBinding

-- | Register a simple hook without a timeout
hookF :: MonadKIO m => HookLocation -> (KeyEvent -> m Catch) -> m ()
hookF l f = register l . Hook Nothing $ \t -> f (t^.event)

-- | Register a hook with a timeout
tHookF :: MonadK m
  => HookLocation         -- ^ Where to install the hook
  -> Milliseconds         -- ^ The timeout delay for the hook
  -> m ()                 -- ^ The action to perform on timeout
  -> (Trigger -> m Catch) -- ^ The action to perform on trigger
  -> m ()                 -- ^ The resulting action
tHookF l d a f = register l $ Hook (Just $ Timeout d a) f

-- | Perform an action after a period of time has elapsed
--
-- This is essentially just a way to perform async actions using the KMonad hook
-- system.
after :: MonadK m
  => Milliseconds
  -> m ()
  -> m ()
after d a = do
  let rehook t = after (d - t^.elapsed) a *> pure NoCatch
  tHookF InputHook d a rehook

-- | Perform an action immediately after the current action is finished. NOTE:
-- there is no guarantee that another event doesn't outrace this, only that it
-- will happen as soon as the CPU gets to it.
whenDone :: MonadK m
  => m ()
  -> m ()
whenDone = after 0


-- | Create a KeyPred that matches the Press or Release of the current button.
matchMy :: MonadK m => Switch -> m KeyPred
matchMy s = (==) <$> my s

-- | Wait for an event to match a predicate and then execute an action
await :: MonadKIO m => KeyPred -> (KeyEvent -> m Catch) -> m ()
await p a = hookF InputHook $ \e -> if p e
  then a e
  else await p a *> pure NoCatch

-- | Execute an action on the detection of the Switch of the active button.
awaitMy :: MonadK m => Switch -> m Catch -> m ()
awaitMy s a = matchMy s >>= flip await (const a)

-- | Try to call a function on a succesful match of a predicate within a certain
-- time period. On a timeout, perform an action.
within :: MonadK m
  => Milliseconds          -- ^ The time within which this filter is active
  -> m KeyPred             -- ^ The predicate used to find a match
  -> m ()                  -- ^ The action to call on timeout
  -> (Trigger -> m Catch)  -- ^ The action to call on a succesful match
  -> m ()                  -- ^ The resulting action
within d p a f = do
  p' <- p
  -- define f' to run action on predicate match, or rehook on predicate mismatch
  let f' t = if p' (t^.event)
        then f t
        else within (d - t^.elapsed) p a f *> pure NoCatch
  tHookF InputHook d a f'

-- | Like `within`, but acquires a hold when starting, and releases when done
withinHeld :: MonadK m
  => Milliseconds          -- ^ The time within which this filter is active
  -> m KeyPred             -- ^ The predicate used to find a match
  -> m ()                  -- ^ The action to call on timeout
  -> (Trigger -> m Catch)  -- ^ The action to call on a succesful match
  -> m ()                  -- ^ The resulting action
withinHeld d p a f = do
  hold True
  within d p (a <* hold False) (\x -> f x <* hold False)
