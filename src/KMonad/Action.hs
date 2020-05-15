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

    -- * Match target
    -- $tgt
  , MatchTarget(..)
  , target
  , capture

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
  = Match KeyEvent -- ^ Match occured on the provided event
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

-- | Whether a match succeeded or not
matched :: HasMatch e => Getter e Bool
matched = match . to (\case
  (Match _) -> True
  _         -> False)

instance Display Match where
  textDisplay m = if m^.matched then "Match" else "NoMatch"

instance Display TimerMatch where
  textDisplay t = if t^.matched
    then "Match in " <> textDisplay (t^.elapsed) <> "ms"
    else "NoMatch"


--------------------------------------------------------------------------------
-- $tgt
--
-- The 'MatchTarget' expresses what to match against in the future. Together with
-- the 'Match' family it is responsible for dealing with KMonad's callback
-- architecture.

-- | The 'KeyEvent' to match against, and whether to `capture` the event
-- (interrupt further processing beside the callback) on a succesful match.
data MatchTarget = MatchTarget
  { _capture :: Bool     -- ^ Whether a match should interrupt further processing.
  , _target  :: KeyEvent -- ^ The 'KeyEvent' to match against
  }
makeLenses ''MatchTarget

instance Display MatchTarget where
  textDisplay h = (<> textDisplay (h^.target)) $ if h^.capture
    then "Catch event: " else "Match event: "


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
  emit        :: KeyEvent -> m ()
  -- | Pause the current thread for n milliseconds
  pause       :: Milliseconds -> m ()
  -- | Pause or unpause event processing
  hold        :: Bool -> m ()
  -- | Run a hook on only the next 'KeyEvent'
  hookNext   :: MatchTarget -> (Match -> m ()) -> m ()
  -- | Run a hook on all 'KeyEvent's until the timer expires,
  hookWithin :: Milliseconds -> MatchTarget -> (TimerMatch -> m ()) -> m ()
  -- | Run a layer-stack manipulation
  layerOp    :: LayerOp -> m ()
  -- | Access the keycode to which the current button is bound
  myBinding  :: m Keycode

--------------------------------------------------------------------------------
-- $combs
--
-- More complicated 'MonadK' operations built from the fundamental
-- components.

-- | Create a KeyEvent matching pressing or releasing of the current button
my :: MonadK m => Switch -> m KeyEvent
my a = mkKeyEvent a <$> myBinding

-- | Create a MatchTarget that matches the Press or Release of the calling button.
matchMy :: MonadK m => Switch -> m MatchTarget
matchMy a = MatchTarget False <$> my a

-- | Create a MatchTarget that catches the Press or Release of the calling button.
catchMy :: MonadK m => Switch -> m MatchTarget
catchMy a = MatchTarget True <$> my a

-- | Wait for an event to match a predicate and then execute an action
await :: MonadK m => m MatchTarget -> (KeyEvent -> m ()) -> m ()
await tgt f = catchNext tgt $ \case
  Match e -> f e
  NoMatch -> await tgt f

-- | Monadic counterpart of hookNext where the predicate runs in MonadK
catchNext :: MonadK m => m MatchTarget -> (Match -> m ()) -> m ()
catchNext tgt f = tgt >>= \tgt' -> hookNext tgt' f

-- | Monadic counterpart of hookWithin where the predicate runs in MonadK
catchWithin :: MonadK m
  => Milliseconds
  -> m MatchTarget
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
  -> m MatchTarget
  -> (TimerMatch -> m ())
  -> m ()
catchWithinHeld ms tgt f = do
  hold True
  catchWithin ms tgt (\e -> f e >>= \res -> hold False >> pure res)
