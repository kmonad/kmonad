-- |

module K.Shell.KeyIO.Env where

import K.Shell.KeyIO.Initial
import K.Shell.KeyIO.LinEvdevSrc
import K.Shell.KeyIO.LinUinputSnk

-- env -------------------------------------------------------------------------

withKeySrc :: (UIO m, CanLog env m) => KeyInputCfg -> (KeySrc -> m a) -> m a
withKeySrc (LinEvdevSrc p) = withLinEvdevSrc p
withKeySrc x = \f -> do
  logError $ tshow x <> " not implemented yet"
  f . KeySrc $ devFail "not implemented yet"

withKeySnk :: (UIO m, CanLog env m) => KeyOutputCfg -> (KeySnk -> m a) -> m a
withKeySnk (LinUinputSnk mn) = withLinUinputSnk mn
withKeySnk x = \f -> do
  logError $ tshow x <> " not implemented yet"
  f . KeySnk $ devFail "not implemented yet"


withKeyRepeatEnv :: (UIO m, CanLog e m) => KeyRepeatCfg -> (KeyRepeatEnv -> m a) -> m a
withKeyRepeatEnv _ f = f KeyRepeatEnv

withKio :: (CanLog e m, UIO m, HasKioCfg c) => c -> Ctx r m KioEnv --(KioEnv -> m a) -> m a
withKio c = ContT $ \f ->
  withKeySrc (c^.keyInputCfg) $ \keysrc ->
    withKeySnk (c^.keyOutputCfg) $ \keysnk ->
      withKeyRepeatEnv (c^.keyRepeatCfg) $ \keyrep ->
        f $ KioEnv keysnk keysrc keyrep

type CanKio e m = (MonadReader e m, HasKioEnv e, MonadIO m)

waitKioEvent :: CanKio e m => m KioEvent
waitKioEvent = view (kioEnv . keySrc) >>= liftIO . srcEvent

sendKioEvent :: CanKio e m => KioEvent -> m ()
sendKioEvent e = view (kioEnv . keySnk) >>= \snk -> liftIO $ snkEvent snk e
