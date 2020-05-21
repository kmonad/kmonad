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
    -- $match
    Match(..)
  , TimerMatch(..)
  , elapsed
  , matched
  , caught
  , succeeded

    -- * Match target
    -- $tgt
  , KeyPred
  , matchWith
  , catchWith

    -- * Layer operations
    -- $lop
  , LayerOp(..)

    -- * MonadK
    -- $monadk
  , MonadK(..)

    -- * Constituted actions
    -- $combs
  , my
  , matchMy
  , catchMy
  , await
  , catchNext
  , catchWithin
  , catchRelease
  , catchRelease_
  , matchRelease
  , matchRelease_
  , catchWithinHeld
  )

where

import KPrelude

import KMonad.Keyboard
import KMonad.Util


--------------------------------------------------------------------------------
-- $match
--
-- To make decisions about what actions to take, we must be able to defer a
-- decision. For example, a TapHold button does not know what action to take
-- until either a timer runs out, or a release is detected. Such deferred
-- judgements are expressed through the family of 'Match' datatypes defined
-- here.

-- | The result of attempting to match, which either succesfully matched against
-- a particular 'KeyEvent', or failed to match.
data Match
  = Match KeyEvent -- ^ Matched but didn't interrupt event
  | Catch KeyEvent -- ^ Matched and interrupted event
  | NoMatch        -- ^ The callback did not match

-- | The result of attempting to match within a certain timeframe, whether the
-- match succeeded or not is contained in the 'tMatch' field, and how much time
-- has elapsed since we started trying to match is contained int he 'elapsed' field.
data TimerMatch = TimerMatch
  { _tMatch  :: Match        -- ^ Result of attempting the match
  , _elapsed :: Milliseconds -- ^ Time since the callback was registered
  }
makeLenses ''TimerMatch

-- | A helper class to speak about things that contain a 'Match'
class HasMatch e where match :: Lens' e Match
instance HasMatch Match      where match = id
instance HasMatch TimerMatch where match = tMatch


-- | Extract the matched event from a value that has a Match
matched :: HasMatch e => Fold e KeyEvent
matched = match . folding (\case
  Match e -> Just e
  Catch e -> Just e
  _       -> Nothing)

-- | Return whether a Match has signalled interrupting an event
caught :: HasMatch e => Getter e Bool
caught = match . to (\case
  Catch _ -> True
  _       -> False)

-- | Return whether a Match succeeded or not
succeeded :: HasMatch e => Getter e Bool
succeeded = match . to (\case
  NoMatch -> False
  _       -> True)

instance Display Match where
  textDisplay m = if m^.succeeded then "Match" else "NoMatch"

instance Display TimerMatch where
  textDisplay t = case t^?matched of
    Just _  -> "Match in " <> textDisplay (t^.elapsed) <> "ms"
    Nothing -> "NoMatch"


--------------------------------------------------------------------------------
-- $tgt
--
-- The 'KeyPred' expresses what to match against in the future. Together with
-- the 'Match' family it is responsible for dealing with KMonad's callback
-- architecture.

-- | The 'KeyEvent' to match against, and whether to `capture` the event
-- (interrupt further processing beside the callback) on a succesful match.
type KeyPred = KeyEvent -> Match

matchWith :: (KeyEvent -> Bool) -> KeyPred
matchWith f = \e -> if f e then Match e else NoMatch

catchWith :: (KeyEvent -> Bool) -> KeyPred
catchWith f = \e -> if f e then Catch e else NoMatch


--------------------------------------------------------------------------------
-- $lop
--
-- Operations that manipulate the layer-stack

data LayerOp
  = PushLayer    LayerTag -- ^ Add a layer to the top of the stack
  | PopLayer     LayerTag -- ^ Remove the first occurence of a layer
  | SetBaseLayer LayerTag -- ^ Change the base-layer


--------------------------------------------------------------------------------
-- $monadk
--
-- The fundamental components that make up any 'Button' operation.

class Monad m => MonadK m where
  -- | Emit a KeyEvent to the OS
  emit       :: KeyEvent -> m ()
  -- | Pause the current thread for n milliseconds
  pause      :: Milliseconds -> m ()
  -- | Pause or unpause event processing
  hold       :: Bool -> m ()
  -- | Run a hook on only the next 'KeyEvent'
  hookNext   :: KeyPred -> (Match -> m ()) -> m ()
  -- | Run a hook on all 'KeyEvent's until the timer expires,
  hookWithin :: Milliseconds -> KeyPred -> (TimerMatch -> m ()) -> m ()
  -- | Run a layer-stack manipulation
  layerOp    :: LayerOp -> m ()
  -- | Access the keycode to which the current button is bound
  myBinding  :: m Keycode

--------------------------------------------------------------------------------
-- $combs
--
-- More complicated 'MonadK' operations built from the fundamental
-- components.

-- TODO: come in here and spruce things up

-- | Create a KeyEvent matching pressing or releasing of the current button
my :: MonadK m => Switch -> m KeyEvent
my a = mkKeyEvent a <$> myBinding

-- | Create a KeyPred that matches the Press or Release of the calling button.
matchMy :: MonadK m => Switch -> m KeyPred
matchMy a = matchWith . (==) <$> my a

-- | Create a KeyPred that catches the Press or Release of the calling button.
catchMy :: MonadK m => Switch -> m KeyPred
catchMy a = catchWith . (==) <$> my a

-- | Wait for an event to match a predicate and then execute an action
await :: MonadK m => m KeyPred -> (KeyEvent -> m ()) -> m ()
await p f = catchNext p $ \m -> case m^?matched of
  Nothing -> await p f
  Just e  -> f e

-- | Monadic counterpart of hookNext where the predicate runs in MonadK
catchNext :: MonadK m => m KeyPred -> (Match -> m ()) -> m ()
catchNext tgt f = tgt >>= \tgt' -> hookNext tgt' f

-- | Monadic counterpart of hookWithin where the predicate runs in MonadK
catchWithin :: MonadK m
  => Milliseconds
  -> m KeyPred
  -> (TimerMatch -> m ())
  -> m ()
catchWithin ms tgt f = tgt >>= \tgt' -> hookWithin ms tgt' f

-- | Run an action on the first occurence of the release of the current button
catchRelease :: MonadK m => (KeyEvent -> m ()) -> m ()
catchRelease f = await (catchMy Release) f

-- | A version of 'onRelease' where the action does not depend on the 'KeyEvent'
catchRelease_ :: MonadK m => m () -> m ()
catchRelease_ = catchRelease . const

-- | Run an action on the first occurence of the release of the current button
matchRelease :: MonadK m => (KeyEvent -> m ()) -> m ()
matchRelease f = await (matchMy Release) f

-- | Run an action on the first occurence of the release of the current button
matchRelease_ :: MonadK m => m () -> m ()
matchRelease_ = matchRelease . const

-- | A 'catchWithin' action which executes its attempt to catch in the context
-- of a paused input stream. This unpauses the input stream the moment
-- 'catchWithin' succeeds to match its predicate, or when the timer expires.
catchWithinHeld :: MonadK m
  => Milliseconds
  -> m KeyPred
  -> (TimerMatch -> m ())
  -> m ()
catchWithinHeld ms tgt f = do
  hold True
  catchWithin ms tgt (\e -> f e >>= \res -> hold False >> pure res)
