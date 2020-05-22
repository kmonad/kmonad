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
  -- ( -- * Matches
  --   -- $match
  --   Match(..)
  -- , TimerMatch(..)
  -- , elapsed
  -- , matched
  -- , succeeded

  --   -- * Match target
  --   -- $tgt
  -- , KeyPred
  -- , matchWith

  --   -- * Layer operations
  --   -- $lop
  -- , LayerOp(..)

  --   -- * MonadK
  --   -- $monadk
  -- , MonadK(..)

  --   -- * Constituted actions
  --   -- $combs
  -- , my
  -- , matchMy
  -- , catchMy
  -- , await
  -- , catchNext
  -- , catchWithin
  -- , catchRelease
  -- , catchRelease_
  -- , matchRelease
  -- , matchRelease_
  -- , catchWithinHeld
  -- )
where

import KPrelude

import KMonad.Keyboard
import KMonad.Util


--------------------------------------------------------------------------------
-- $pred

-- | Whether a predicate matched on a 'KeyEvent' or not.
data Match = Match KeyEvent | NoMatch deriving (Eq, Show)

-- | A 'KeyPred' is a function that calculates matches against events.
type KeyPred = KeyEvent -> Match

-- | Create a 'KeyPred' from a predicate on 'KeyEvent'
matchOn :: (KeyEvent -> Bool) -> KeyPred
matchOn p = \e -> if p e then Match e else NoMatch

-- | Create a 'KeyPred' that matches only an exact event
matchEvent :: KeyEvent -> KeyPred
matchEvent = matchOn . (==)

-- | A 'KeyPred' that will match on any 'Press' event
matchPress :: KeyPred
matchPress = matchOn $ (Press ==) . view switch




--------------------------------------------------------------------------------
-- $cb

-- | The 'Catch' type is used to signal whether an event should be intercepted.
data Catch = Catch | NoCatch deriving (Eq, Show)

instance Semigroup Catch where
  NoCatch <> NoCatch = NoCatch
  _       <> _       = Catch

instance Monoid Catch where
  mempty = NoCatch

-- | A 'Callback' is a function that is called on match results
type Callback m = Match -> m Catch



-- -- | The result of attempting to match within a certain timeframe, whether the
-- -- match succeeded or not is contained in the 'tMatch' field, and how much time
-- -- has elapsed since we started trying to match is contained in the 'elapsed' field.
-- data TimerMatch = TimerMatch
--   { _tMatch  :: Match        -- ^ Result of attempting the match
--   , _elapsed :: Milliseconds -- ^ Time since the callback was registered
--   }
-- makeLenses ''TimerMatch

-- -- | A helper class to speak about things that contain a 'Match'
-- class HasMatch e where match :: Lens' e Match
-- instance HasMatch Match      where match = id
-- instance HasMatch TimerMatch where match = tMatch

-- -- | Extract the matched event from a value that has a Match
-- matched :: HasMatch e => Fold e KeyEvent
-- matched = match . folding (\case
--   Match   e -> Just e
--   NoMatch   -> Nothing)

-- -- | Return whether a Match succeeded or not
-- succeeded :: HasMatch e => Getter e Bool
-- succeeded = match . to (\case
--   NoMatch -> False
--   _       -> True)

-- instance Display Match where
--   textDisplay = tshow

-- instance Display TimerMatch where
--   textDisplay t = case t^?matched of
--     Just _  -> "Match in " <> textDisplay (t^.elapsed) <> "ms"
--     Nothing -> "NoMatch"


--------------------------------------------------------------------------------
-- $tgt
--
-- The 'KeyPred' expresses what to match against in the future. Together with
-- the 'Match' family it is responsible for dealing with KMonad's callback
-- architecture.

-- | The 'KeyEvent' to match against, and whether to `capture` the event
-- (interrupt further processing beside the callback) on a succesful match.
-- type KeyPred = KeyEvent -> Match

-- -- | Capture, used in functions that generates 'KeyPred's, used to indicate
-- -- whether a KeyPred should capture or not.
-- data Catch = DoCatch | NoCatch deriving (Eq, Show)


-- -- | Create a 'KeyPred' that will
-- onEvent :: Capture -> KeyEvent -> KeyPred
-- onEvent c e = \e' -> if (e == e') then
--   case c of
--     DoCatch -> Catch e
--     NoCatch -> Match e
--   else NoMatch

-- matchWith :: (KeyEvent -> Bool) -> KeyPred
-- matchWith f = \e -> if f e then Match e else NoMatch

-- catchWith :: (KeyEvent -> Bool) -> KeyPred
-- catchWith f = \e -> if f e then Catch e else NoMatch


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
  hookNext   :: KeyPred -> Callback m -> m ()
  -- | Run a hook on all 'KeyEvent's until the timer expires,
  hookWithin :: Milliseconds -> KeyPred -> Callback m -> m ()
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
my s = mkKeyEvent s <$> myBinding

-- | Create a KeyPred that matches the Press or Release of the calling button.
matchMy :: MonadK m => Switch -> m KeyPred
matchMy s = matchEvent <$> my s

-- | Wait for an event to match a predicate and then execute an action
await :: MonadK m => KeyPred -> (KeyEvent -> m Catch) -> m ()
await p f = hookNext p $ \case
  Match e -> f e
  NoMatch -> await p f *> pure NoCatch

-- | Execute an action on the detection of the Switch of the active button.
--
-- Use this, for example, to register a callback from a 'pressAction' to
-- explicitly handle the 'Release' of the button.
--
-- NOTE: There is no reason to include the `Match ->` part of the 'Callback'
-- since this will only ever be run on an Event that we already know.
awaitMy :: MonadK m => Switch -> m Catch -> m ()
awaitMy s a = matchMy s >>= \p -> await p (const a)

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

-- | Transform a match-handler so that, whenever a 'Match' is detected, a
-- 'Catch' is signalled, and vice-versa. This is essentially the default
-- catching behavior, and this function just makes it easy to add it default
-- catching behavior to handlers.
catchMatch :: MonadK m => (Match -> m a) -> Callback m
catchMatch h = \m -> h m *> case m of
  Match _ -> pure Catch
  NoMatch -> pure NoCatch
 
-- -- | Monadic counterpart of hookWithin where the predicate runs in MonadK
-- catchWithin :: MonadK m
--   => Milliseconds
--   -> m KeyPred
--   -> (TimerMatch -> m ())
--   -> m ()
-- catchWithin ms tgt f = tgt >>= \tgt' -> hookWithin ms tgt' f

-- -- | Run an action on the first occurence of the release of the current button
-- catchRelease :: MonadK m => (KeyEvent -> m ()) -> m ()
-- catchRelease f = await (catchMy Release) f

-- -- | A version of 'onRelease' where the action does not depend on the 'KeyEvent'
-- catchRelease_ :: MonadK m => m () -> m ()
-- catchRelease_ = catchRelease . const

-- -- | Run an action on the first occurence of the release of the current button
-- matchRelease :: MonadK m => (KeyEvent -> m ()) -> m ()
-- matchRelease f = await (matchMy Release) f

-- -- | Run an action on the first occurence of the release of the current button
-- matchRelease_ :: MonadK m => m () -> m ()
-- matchRelease_ = matchRelease . const

-- | A 'catchWithin' action which executes its attempt to catch in the context
-- of a paused input stream. This unpauses the input stream the moment
-- 'catchWithin' succeeds to match its predicate, or when the timer expires.
hookWithinMHeld :: MonadK m
  => Milliseconds
  -> m KeyPred
  -> Callback m
  -> m ()
hookWithinMHeld ms p f = hold True *> hookWithinM ms p (\e -> f e <* hold False)
