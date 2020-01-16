module KMonad.Action

where

import Prelude

import KMonad.Keyboard
import KMonad.Util


--------------------------------------------------------------------------------
-- $pred
--
-- The types and functions dealing with functions that try to match key-events.

type MB a = forall m. MonadButton m => m a

data Match
  = Match KeyEvent -- Match but no capture
  | Catch KeyEvent -- Match and capture
  | NoMatch        -- No match
  deriving Show

instance Display Match where
  textDisplay (Match e) = "Match: " <> textDisplay e
  textDisplay (Catch e) = "Catch: " <> textDisplay e
  textDisplay NoMatch   = "NoMatch"

matched :: Getter Match Bool
matched = to $ \case
  NoMatch -> False
  _       -> True

caught :: Getter Match Bool
caught = to $ \case
  Catch _ -> True
  _       -> False

data HookPred
  = MatchOn KeyAction
  | CatchOn KeyAction

instance Display HookPred where
  textDisplay (MatchOn a) = "MatchOn: " <> textDisplay a
  textDisplay (CatchOn a) = "CatchOn: " <> textDisplay a

runPred :: HookPred -> KeyEvent -> Match
runPred (MatchOn a) e = bool NoMatch (Match e) (e `kaEq` a)
runPred (CatchOn a) e = bool NoMatch (Catch e) (e `kaEq` a)

-- | Turn a simple predicate into a HookPred that matches a KeyEvent.
-- matchKey :: (KeyEvent -> Bool) -> HookPred
-- matchKey p = \e -> bool NoMatch (Match e) $ p e

-- | Turn a simple predicate into a HookPred that captures a KeyEvent.
-- catchKey :: (KeyEvent -> Bool) -> HookPred
-- catchKey p = \e -> bool NoMatch (Catch e) $ p e

-- | Create a HookPred that matches the Press or Release of the calling button.
matchMy :: MonadButton m => SwitchAction -> m HookPred
matchMy a = MatchOn <$> my a

-- | Create a HookPred that catches the Press or Release of the calling button.
catchMy :: MonadButton m => SwitchAction -> m HookPred
catchMy a = CatchOn <$> my a
  -- my a >>= \b -> pure $ \e -> bool NoMatch (Catch e) (e `kaEq` b)

-- | A HookFun is a function that is run on the result of trying to 'Match'
type HookFun m = Match -> m ()

--------------------------------------------------------------------------------
-- $monad
--
-- Most of KMonad is written in RIO style, but we provide a set of actions in
-- 'MonadButton' that exposes a programmable API to the user, without exposing 'IO'.

-- FIXME: remove MonadIO when finished debugging
-- class Monad m => MonadButton m where
class MonadIO m => MonadButton m where
  -- | Emit a KeyAction to the OS
  emit        :: KeyAction -> m ()
  -- | Pause the current thread for n milliseconds
  pause       :: Milliseconds -> m ()
  -- | Pause or unpause event processing
  hold        :: Bool -> m ()
  -- | Run a hook on only the next 'KeyEvent'
  hookNext   :: HookPred -> HookFun m -> m ()
  -- | Run a hook on all 'KeyEvent's until the timer expires,
  hookWithin :: Milliseconds -> HookPred -> HookFun m -> m ()
  -- | Access the keycode to which this button is bound
  myBinding   :: m Keycode

--------------------------------------------------------------------------------
-- $action
--
-- Action wrapper for creating Buttons
--

newtype Action a = Action { _runAction :: MB a}
  deriving (Functor)

runAction :: MonadButton m => Action a -> m a
runAction = _runAction

mkAction :: MB a -> Action a
mkAction = Action

instance Applicative Action where
  pure a  = Action $ (pure a)
  (Action f) <*> (Action a)
    = Action $ f <*> a


--------------------------------------------------------------------------------
-- $combs
--
-- Combinators derived from the MonadButton primitives
--

-- | Create a KeyAction matching pressing or releasing of the current button
my :: MonadButton m => SwitchAction -> m KeyAction
my a = mkKeyAction a <$> myBinding

-- | Wait for an event to match a predicate and then execute an action
await :: MonadButton m => m HookPred -> (KeyEvent -> m ()) -> m ()
await p f = catchNext p $ \case
  Match e -> f e
  Catch e -> f e
  NoMatch -> await p f

-- | Monadic counterpart of hookNext where the predicate runs in MonadButton
catchNext :: MonadButton m => m HookPred -> HookFun m -> m ()
catchNext p f = p >>= \p' -> hookNext p' f

-- | Monadic counterpart of hookWithin where the predicate runs in MonadButton
catchWithin :: MonadButton m => Milliseconds -> m HookPred -> HookFun m -> m ()
catchWithin ms p f = p >>= \p' -> hookWithin ms p' f

-- | Run an action on the first occurence of the release of the current button
catchRelease :: MonadButton m => (KeyEvent -> m ()) -> m ()
catchRelease f = await (catchMy Release) f

-- | A version of 'onRelease' where the action does not depend on the 'KeyEvent'
catchRelease_ :: MonadButton m => m () -> m ()
catchRelease_ = catchRelease . const

-- | Run an action on the first occurence of the release of the current button
matchRelease :: MonadButton m => (KeyEvent -> m ()) -> m ()
matchRelease f = await (matchMy Release) f

-- | Run an action on the first occurence of the release of the current button
matchRelease_ :: MonadButton m => m () -> m ()
matchRelease_ = matchRelease . const

-- | A 'catchWithin' action which executes its attempt to catch in the context
-- of a paused input stream. This unpauses the input stream the moment
-- 'catchWithin' succeeds to match its predicate, or when the timer expires.
catchWithinHeld :: MonadButton m => Milliseconds -> m HookPred -> HookFun m -> m ()
catchWithinHeld ms p f = do
  hold True
  catchWithin ms p (\e -> f e >>= \res -> hold False >> pure res)
