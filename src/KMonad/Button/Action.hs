module KMonad.Button.Action

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
  = Match KeyEvent
  | NoMatch

matched :: Getter Match Bool
matched = to $ \case
  Match _ -> True
  _       -> False

type HookPred = KeyEvent -> Match

-- | Turn a simple Predicate on KeyEvent into a HookPred
match :: (KeyEvent -> Bool) -> HookPred
match p = \e -> bool (Match e) NoMatch $ p e

-- | Create a HookPred that matches the Press or Release of the calling button.
matchMy :: MonadButton m => SwitchAction -> m HookPred
matchMy a = my a >>= \b -> pure $ \e -> bool (Match e) (NoMatch) (e `kaEq` b)

-- | A HookFun is a function that is run on the result of trying to 'Match'
type HookFun = Match -> MB ()

--------------------------------------------------------------------------------
-- $monad
--
-- Most of KMonad is written in RIO style, but we provide a set of actions in
-- 'MonadButton' that exposes a programmable API to the user, without exposing 'IO'.

class Monad m => MonadButton m where
  -- | Emit a KeyAction to the OS
  emit        :: KeyAction -> m ()
  -- | Pause the current thread for n milliseconds
  pause       :: Milliseconds -> m ()
  -- | Pause or unpause event processing
  hold        :: Bool -> m ()
  -- | Run a hook on only the next 'KeyEvent'
  hookNext   :: HookPred -> HookFun -> m ()
  -- | Run a hook on all 'KeyEvent's until the timer expires,
  hookWithin :: Milliseconds -> HookPred -> HookFun -> m ()
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
await :: MonadButton m => MB HookPred -> (KeyEvent -> MB ()) -> m ()
await p f = catchNext p $ \case
  Match e -> f e
  NoMatch -> await p f

-- | Monadic counterpart of hookNext where the predicate runs in MonadButton
catchNext :: MonadButton m => m HookPred -> HookFun -> m ()
catchNext p f = p >>= \p' -> hookNext p' f

-- | Monadic counterpart of hookWithin where the predicate runs in MonadButton
catchWithin :: MonadButton m => Milliseconds -> m HookPred -> HookFun -> m ()
catchWithin ms p f = p >>= \p' -> hookWithin ms p' f

-- | Run an action on the first occurence of the release of the current button
onRelease :: MonadButton m => (KeyEvent -> MB ()) -> m ()
onRelease f = await (matchMy Release) f

-- | A 'catchWithin' action which executes its attempt to catch in the context
-- of a paused input stream. This unpauses the input stream the moment
-- 'catchWithin' succeeds to match its predicate, or when the timer expires.
catchWithinHeld :: MonadButton m => Milliseconds -> m HookPred -> HookFun -> m ()
catchWithinHeld ms p f = do
  hold True
  catchWithin ms p (\e -> f e >>= \res -> hold False >> pure res)
