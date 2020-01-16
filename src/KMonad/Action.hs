module KMonad.Action

where

import KPrelude

import KMonad.Keyboard
import KMonad.Util


--------------------------------------------------------------------------------
-- $pred
--
-- The types and functions dealing with functions that try to match key-events.


data Match
  = Match   KeyEvent
  | NoMatch

class HasMatch e where match :: Lens' e Match
instance HasMatch Match where match = id

data TimerMatch = TimerMatch
  { _tMatch  :: Match
  , _elapsed :: Milliseconds
  }
makeLenses ''TimerMatch

instance HasMatch TimerMatch where match = tMatch

matched :: HasMatch e => Getter e Bool
matched = match . to (\case
  (Match _) -> True
  _         -> False)

-- class HasMatched e where matched :: Lens' e Bool
-- instance HasMatched NextMatch where matched = nMatched
-- instance HasMatched TimerMatch where matched = tMatched

instance Display Match where
  textDisplay m = if m^.matched then "Match" else "NoMatch"

instance Display TimerMatch where
  textDisplay t = if t^.matched
    then "Match in " <> textDisplay (t^.elapsed) <> "ms"
    else "NoMatch"

data HookPred = HookPred
  { _capture :: Bool
  , _target  :: KeyAction
  }
makeLenses ''HookPred

instance Display HookPred where
  textDisplay h = (<> textDisplay (h^.target)) $ if h^.capture
    then "Catch event: " else "Match event: "

-- | Create a HookPred that matches the Press or Release of the calling button.
matchMy :: MonadButton m => SwitchAction -> m HookPred
matchMy a = HookPred False <$> my a

-- | Create a HookPred that catches the Press or Release of the calling button.
catchMy :: MonadButton m => SwitchAction -> m HookPred
catchMy a = HookPred True <$> my a

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
  hookNext   :: HookPred -> (Match -> m ()) -> m ()
  -- | Run a hook on all 'KeyEvent's until the timer expires,
  hookWithin :: Milliseconds -> HookPred -> (TimerMatch -> m ()) -> m ()
  -- | Access the keycode to which this button is bound
  myBinding   :: m Keycode

--------------------------------------------------------------------------------
-- $action
--
-- Action wrapper for creating Buttons
--

type MB a = forall m. MonadButton m => m a
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
  NoMatch -> await p f

-- | Monadic counterpart of hookNext where the predicate runs in MonadButton
catchNext :: MonadButton m => m HookPred -> (Match -> m ()) -> m ()
catchNext p f = p >>= \p' -> hookNext p' f

-- | Monadic counterpart of hookWithin where the predicate runs in MonadButton
catchWithin :: MonadButton m
  => Milliseconds
  -> m HookPred
  -> (TimerMatch -> m ())
  -> m ()
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
catchWithinHeld :: MonadButton m
  => Milliseconds
  -> m HookPred
  -> (TimerMatch -> m ())
  -> m ()
catchWithinHeld ms p f = do
  hold True
  catchWithin ms p (\e -> f e >>= \res -> hold False >> pure res)
