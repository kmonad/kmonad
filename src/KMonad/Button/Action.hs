module KMonad.Button.Action

where

import Prelude

import KMonad.Keyboard
import KMonad.Util

--------------------------------------------------------------------------------
-- $match




-- type Captured = Bool

-- capture :: MonadButton m => m Captured
-- capture = pure True

-- noCapture :: MonadButton m => m Captured
-- noCapture = pure False

-- isCapture :: Captured -> Bool
-- isCapture = id

-- -- TODO: Come in here and make it nice

-- type KeyPredFun   = forall m. MonadButton m => KeyEvent -> m (Maybe KeyEvent)
-- type KeyHdlrFun f = forall m. MonadButton m => f KeyEvent -> (Captured, m ())

-- newtype KeyPred = KPred  { runPred  :: KeyPredFun }
-- newtype KeyHdlr = KHdlr { runHdlrM :: KHdlrFunM }



-- handleWith :: KHdlrFunM -> KHdlr
-- handleWith = KeyHandler

-- type CatchTest   =
-- type CatchResult = forall m. MonadButton m => (Captured, m ())

--------------------------------------------------------------------------------
-- $match
--
-- The types and functions dealing with how we represent matches and captures.

-- newtype Match = Match (Maybe Bool)
-- makeWrapped ''Match

-- matched :: Getter Match Bool
-- matched = to isJust

-- captured :: Getter Match Bool
-- captured = to $ fromMaybe False

-- capture :: Match
-- capture = Match $ Just True

-- noCapture :: Match
-- noCapture = Match $ Just False

-- noMatch :: Match
-- noMatch = Match $ Nothing

--------------------------------------------------------------------------------
-- $pred
--
-- The types and functions dealing with functions that try to match key-events.

type MB a = forall m. MonadButton m => m a



data Match
  = Match KeyEvent
  | NoMatch

type HookPred =  KeyEvent -> MB Match

-- | Turn a simple Predicate on KeyEvent into a HookPred
match :: (KeyEvent -> Bool) -> HookPred
match p = \e -> pure . bool (Match e) NoMatch $ p e

-- | Create a HookPred that matches the Press or Release of the calling button.
matchMy :: SwitchAction -> HookPred
matchMy a = \e -> bool (Match e) (NoMatch) . (e `kaEq`) <$> my a

-- | A HookFun is a function that is run on the result of trying to 'Match'
type HookFun = Match -> MB ()


--------------------------------------------------------------------------------
-- $monad
--
-- Most of KMonad is written in RIO style, but we provide a set of actions in
-- 'MonadButton' that exposes a programmable API to the user, without exposing 'IO'.

-- MonadIO should only be enabled during debugging, for traceIO statements and such
-- class Monad m => MonadButton m where
class Monad m => MonadButton m where
  -- | Emit a KeyAction to the OS
  emit        :: KeyAction -> m ()
  -- | Pause the current thread for n milliseconds
  pause       :: Milliseconds -> m ()
  -- | Pause or unpause event processing
  hold        :: Bool -> m ()
  -- | Run a 'Snare' on only the next 'KeyEvent'
  catchNext   :: HookPred -> HookFun -> m ()
  -- | Run a 'Snare' on all 'KeyEvent's until the timer expires,
  catchWithin :: Milliseconds -> HookPred -> HookFun -> m ()
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
await :: MonadButton m => HookPred -> (KeyEvent -> MB ()) -> m ()
await p f = catchNext p $ \case
  Match e -> f e
  NoMatch -> await p f

-- | Run an action on the first occurence of the release of the current button
onRelease :: MonadButton m => (KeyEvent -> MB ()) -> m ()
onRelease f = await (matchMy Release) f

-- | A 'catchWithin' action which executes its attempt to catch in the context
-- of a paused input stream.
catchWithinHeld :: MonadButton m => Milliseconds -> HookPred -> HookFun -> m ()
catchWithinHeld ms p f = do
  hold True
  catchWithin ms p (\e -> f e >>= \res -> hold False >> pure res)
