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
  ( -- * Matches
    -- $pred
    Match(..)
  , KeyPred
  , matchOn
  , matchEvent
  , matchPress

    -- * Match target
    -- $cb
  , Catch(..)
  , Callback

    -- * Layer operations
    -- $lop
  , LayerOp(..)

    -- * MonadK
    -- $monadk
  , MonadK(..)
  , AnyK
  , Action(..)

    -- * Constituted actions
    -- $combs
  , my
  , matchMy
  , catchMatch
  , await
  , awaitMy
  , hookNextM
  , hookWithinM
  , hookWithinHeldM
  )

where

import KMonad.Prelude

import KMonad.Keyboard
import KMonad.Util



--------------------------------------------------------------------------------
-- $pred


-- | Whether a predicate matched on a 'KeyEvent' or not.
-- data Match = Match KeyEvent | NoMatch deriving (Eq, Show)

-- | A 'KeyPred' is a function that calculates matches against events.
type KeyPred = KeyEvent -> Bool

-- -- | Create a 'KeyPred' that matches only an exact event
-- matchEvent :: KeyEvent -> KeyPred
-- matchEvent = matchOn . (==)

-- -- | A 'KeyPred' that will match on any 'Press' event
-- matchPress :: KeyPred
-- matchPress = matchOn $ (Press ==) . view switch


--------------------------------------------------------------------------------
-- $cb

-- | All the different things that can trigger a Callback
data Trigger
  = Match   KeyEvent -- ^ Predicate matched on a `KeyEvent`
  | NoMatch KeyEvent -- ^ Predicate caught but failed to match on a `KeyEvent`
  | Timeout          -- ^ Timeout of the callback was thrown
  deriving (Eq, Show)
 
-- | The 'Catch' type is used to signal whether an event should be intercepted.
data Catch
  = Catch   -- ^ Interrupt any further processing of the current `KeyEvent`
  | NoCatch -- ^ Continue processing the current `KeyEvent`
  deriving (Eq, Show)

-- | The Semigroup instance of Catch is /any 'Catch' implies 'Catch'/
instance Semigroup Catch where
  NoCatch <> NoCatch = NoCatch
  _       <> _       = Catch

-- | The default is not to 'Catch'
instance Monoid Catch where
  mempty = NoCatch

-- | A 'Callback' is a function from a 'Trigger' to a 'Catch' in some context
type Callback m = Trigger -> m Catch

--------------------------------------------------------------------------------
-- $hook

-- | A 'Hook' value contains all the information necessary to run a hook.
data Hook m = Hook
  { hPred     :: KeyPred            -- ^ The predicate used to determine a match
  , hCallback :: Callback m         -- ^ The callback to perform on a match
  , hTimeout  :: Maybe Milliseconds -- ^ An optional timeout
  }

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
class Monad m => MonadK m where
  -- | Emit a KeyEvent to the OS
  emit       :: KeyEvent -> m ()
  -- | Pause the current thread for n milliseconds
  pause      :: Milliseconds -> m ()
  -- | Pause or unpause event processing
  hold       :: Bool -> m ()
  -- | Register a callback hook
  hook       :: Hook m -> m ()
  -- | Run a layer-stack manipulation
  layerOp    :: LayerOp -> m ()
  -- | Access the keycode to which the current button is bound
  myBinding  :: m Keycode

-- | Type alias for `any monad that can perform MonadK actions`
type AnyK a = forall m. MonadK m => m a

-- | A newtype wrapper used to construct 'MonadK' actions
newtype Action = Action { runAction :: AnyK ()}

--------------------------------------------------------------------------------
-- $combs
--
-- More complicated 'MonadK' operations built from the fundamental
-- components.

-- | Create a KeyEvent matching pressing or releasing of the current button
my :: MonadK m => Switch -> m KeyEvent
my s = mkKeyEvent s <$> myBinding

-- | Create a KeyPred that matches the Press or Release of the calling button.
matchMy :: MonadK m => Switch -> m KeyPred
matchMy s = (==) <$> my s

-- | Wait for an event to match a predicate and then execute an action
await :: MonadK m => KeyPred -> (KeyEvent -> m Catch) -> m ()
await p f = hookNext p $ \case
  Match   e -> f e
  NoMatch _ -> await p f *> pure NoCatch

-- | Execute an action on the detection of the Switch of the active button.
--
-- Use this, for example, to register a callback from a
-- 'KMonad.Button._pressAction' to explicitly handle the 'Release' of the
-- button.
--
-- NOTE: There is no reason to include the @Match ->@ part of the 'Callback'
-- since this will only ever be run on an 'KeyEvent' that we already know.
awaitMy :: MonadK m => Switch -> m Catch -> m ()
awaitMy s a = matchMy s >>= \p -> await p (const a)

-- | Transform a match-handler so that, whenever a 'Match' is detected, a
-- 'Catch' is signalled, and vice-versa. This is essentially the default
-- catching behavior, and this function just makes it easy to add it default
-- catching behavior to handlers.
catchMatch :: MonadK m => (Trigger -> m a) -> Callback m
catchMatch h = \m -> h m *> case m of
  Match   _ -> pure Catch
  NoMatch _ -> pure NoCatch
  Timeout   -> pure NoCatch

-- | 'hookNext' except the 'KeyPred' runs in 'MonadK'
hookNextM :: MonadK m => m KeyPred -> Callback m -> m ()
hookNextM p c = p >>= \p' -> hookNext p' c

-- | 'hookWithin' except the 'KeyPred' runs in 'MonadK'
hookWithinM :: MonadK m
  => Milliseconds
  -> m KeyPred
  -> Callback m
  -> m ()
hookWithinM ms p c = p >>= \p' -> hookWithin ms p' c

-- | A 'hookWithinM' action which executes its attempt to catch in the context
-- of a paused input stream. This unpauses the input stream the moment
-- 'hookWithinM' succeeds to match its predicate, or when the timer expires.
hookWithinHeldM :: MonadK m
  => Milliseconds
  -> m KeyPred
  -> Callback m
  -> m ()
hookWithinHeldM ms p f = hold True *> hookWithinM ms p (\e -> f e <* hold False)
