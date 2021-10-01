-- |

module KMonad.App.KeyIO.Common.IO where

import KMonad.Prelude

import KMonad.Util.Ctx
import KMonad.Util.Keyboard
import KMonad.Util.Logging
import KMonad.Util.Time

--------------------------------------------------------------------------------
-- $typ
--
-- The configuration and run-env for key-repeat

-- | Configuration record for key-repeat tool
data RepeatCfg = RepeatCfg
  { _delay    :: Ms                   -- ^ How long to wait before starting to repeat
  , _interval :: Ms                   -- ^ How long to wait between repeats
  , _repeatIO :: Keycode -> OnlyIO () -- ^ How to send repeat events for some keycode
  }
makeClassy ''RepeatCfg

-- | Run-env for key-repeat tool
data RepeatEnv = RepeatEnv
  { _cfg   :: RepeatCfg
  , _vProc :: MVar (Maybe (Keycode, Async ()))
  }
makeClassy ''RepeatEnv

instance HasRepeatCfg RepeatEnv where repeatCfg = cfg

type CanRepeat m env = (LUIO m env, HasRepeatEnv env)

-- | Shorthand
type R a = RIO RepeatEnv a

--------------------------------------------------------------------------------
-- $op
--
-- The inside-env operations for key-repeat

-- | Do the repeat action on some keycode forever
--
-- NOTE: This function never terminates and should be forked off using async.
repeatKey :: Keycode -> R ()
repeatKey c = do
  sendKey <- liftIO . ($ c) <$> view repeatIO
  (wait =<< view delay) >> sendKey
  forever $ (wait =<< view interval) >> sendKey



--------------------------------------------------------------------------------
-- $api

-- | Start repeating some key, potentially stopping some preexisting repeater
startRepeat :: (CanRepeat m env, HasCode c) => c -> m ()
startRepeat c = view repeatEnv >>= \env -> runRIO env $
  view vProc >>= \v -> modifyMVar v $ \ma -> do
    maybe (pure ()) (cancel . snd) ma
    a <- async . repeatKey $ c^.code
    pure (Just(c^.code, a), ())

-- | Stop repeating some key if it matches the provided keycode, else do nothing
stopRepeat :: (CanRepeat m env, HasCode c) => c -> m ()
stopRepeat c = view repeatEnv >>= \env -> runRIO env $
  view vProc >>= \v -> modifyMVar v $ \case
    Nothing                  -> pure (Nothing, ())
    Just (c', a) | (c^.code) == (c'^.code) -> cancel a >> pure (Nothing, ())
                 | otherwise               -> pure (Just (c', a), ())

-- | Context for key-repeat tool
withRepeat :: (LUIO m env)
  => RepeatCfg -> Ctx r m RepeatEnv
withRepeat c = mkCtx $ \f -> do
  let init = RepeatEnv c <$> newMVar Nothing
  let cleanup env = readMVar (env^.vProc) >>= maybe (pure ()) (cancel . snd)
  bracket init cleanup $ \env -> f env
