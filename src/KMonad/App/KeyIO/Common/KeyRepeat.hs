-- NOTE: A tricky thing about this module: you can't just put the 'KeyRepeatEnv'
-- into whatever Env is required for OS KeyIO. The reason being: we need an
-- OnlyIO action to emit events. This depends on an initialized and running
-- KeyIO env. That would make this circular.
--
-- The solution I took is to initialize all other fields of the KeyIO-env first,
-- while setting the KeyRepeat part to undefined. Then we can 'runRIO' the env
-- already, so long as we never touch the KeyRepeat part. Then `_actIO` can be
-- defined in those modules and passed to fully initialize KeyRepeat.
--
-- This is a bit hacky, and there might be an argument to be made for separating
-- KeyIO into 'core KeyIO', which is only how to get and put events, and
-- 'general KeyIO' which adds stuff like KeyRepeat and input filtering. That
-- would require more boilerplate though, so not right now, but maybe in the future.
--
-- NOTE: The above pattern is interesting though: the fact that 1 abstraction
-- depends on another abstraction being partially defined means that the first
-- abstraction is lacking 'granularity of definition'. However, too much
-- 'granularity' of definition makes the code unwieldy. Tradeoffs.

-- |
module KMonad.App.KeyIO.Common.KeyRepeat where

import KMonad.App.KeyIO.Common.Types
import KMonad.Prelude
import KMonad.Util.Ctx
import KMonad.Util.Keyboard
import KMonad.Util.Logging
import KMonad.Util.Time

--------------------------------------------------------------------------------

-- $typ
--
-- The configuration and run-env for key-repeat

-- | Our state of 'perhaps currently running some thread for some keycode'
type Proc = Maybe (Keycode, Async ())

-- | Run-env for key-repeat tool
data RepeatEnv = RepeatEnv
  { _krCfg :: KeyRepeatCfg,
    _emitRep :: Keycode -> OnlyIO (),
    _vProc :: MVar Proc
  }

makeClassy ''RepeatEnv

-- | Lens into the `KeyRepeatCfg`
instance HasKeyRepeatCfg RepeatEnv where keyRepeatCfg = krCfg

-- | Constraint shorthand
type CanRepeat m env = (LUIO m env, HasRepeatEnv env)

-- | Type shorthand
type R a = RIO RepeatEnv a

--------------------------------------------------------------------------------

-- | Run some state-update
overProc :: (Proc -> R Proc) -> R ()
overProc f = view vProc >>= \v -> modifyMVar v $ fmap (,()) . f

-- | Repeat some key forever
--
-- NOTE: This never terminates and is intended to be forked off with async
repKey :: Keycode -> R ()
repKey c = do
  let go t = wait t >> view emitRep >>= liftIO . ($ c)
  go =<< view delay
  forever $ go =<< view interval

-- | Handle a request to repeat a key
--
-- NOTE: We rely on a simplification here: there are technically 3 scenarios
-- that could be occurring when we are requested to handle a KeySwitch:
-- 1. We are doing nothing -> We start repeating
-- 2. We are repping some other key -> We replace with repping this
-- 3. We are already repping this key -> We should do nothing.
--
-- HOWEVER: 3. should be unreachable. Since KMonad internally only represents
-- toggles of press-states, we should never, ever receive `Pa Pa`.
--
-- This is a good model constraint to simplify thinking, but would be great if
-- it could be reinforced in the types or in the code.
handlePress :: Keycode -> R ()
handlePress c = overProc $ \x -> do
  maybe (pure ()) (cancel . snd) x
  a <- async $ repKey c
  pure $ Just (c, a)

-- | Handle a request to release a key
--
-- If we are currently repeating this keycode: stop doing so. Otherwise, leave
-- everything unchanged.
handleRelease :: Keycode -> R ()
handleRelease c = overProc $ \case
  Just (x, a) | c == x -> cancel a >> pure Nothing
  p -> pure p

--------------------------------------------------------------------------------

-- $api

-- | Return a RepeatEnv
--
-- NOTE: We rely on 'Async's auto-cleanup to stop the repeater thread on
-- termination. Therefore there is no need for explicit cleanup. Otherwise, this
-- would have to be a 'Ctx'.
mkRepeatEnv :: LUIO m env => KeyRepeatCfg -> (Keycode -> OnlyIO ()) -> m RepeatEnv
mkRepeatEnv cfg emitRep = RepeatEnv cfg emitRep <$> newMVar Nothing

-- | Update the repeater state by something that contains a KeySwitch
handleRepeat :: (CanRepeat m env, HasKeySwitch e) => e -> m ()
handleRepeat e =
  view repeatEnv >>= \env ->
    runRIO env $
      let s = e ^. keySwitch
       in if isPress s then handlePress $ s ^. code else handleRelease $ s ^. code
