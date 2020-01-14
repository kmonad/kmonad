{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module KMonad.Daemon
  ( DaemonCfg(..)
  , runDaemon
  , startDaemon
  )
where

import Prelude


import KMonad.Button
import KMonad.Event
import KMonad.Keyboard
import KMonad.Keyboard.IO
import KMonad.Util
import KMonad.Runner

import qualified UnliftIO.Async           as A
import qualified KMonad.Daemon.Dispatch   as Di


--------------------------------------------------------------------------------

-- | All the configuration options for running a daemon
data DaemonCfg = DaemonCfg
  { _keySinkDev   :: Acquire KeySink
  , _keySourceDev :: Acquire KeySource
  , _keymapCfg    :: Keymap Button
  , _port         :: ()
  }
makeClassy ''DaemonCfg

-- | The running environment of the Daemon
data DaemonEnv = DaemonEnv
  { _deDispatch   :: Di.Dispatch
  , _deKeymap     :: MVar (Keymap ButtonEnv)
  , _deDaemonCfg  :: DaemonCfg
  , _deKeySink    :: KeySink
  , _deRunEnv     :: RunEnv
  }
makeLenses ''DaemonEnv

-- | RIO constraints for being able to perform daemon actions
class ( HasLogFunc e , Di.HasDispatch e , HasKeymap e
      , HasKeySink e , HasRunEnv e ) => HasDaemonEnv e where
  daemonEnv :: Lens' e DaemonEnv

instance HasDaemonEnv DaemonEnv where daemonEnv = id

instance {-# OVERLAPS #-} (HasDaemonEnv e) => HasDaemonCfg e where
  daemonCfg = daemonEnv . deDaemonCfg
instance {-# OVERLAPS #-} (HasDaemonEnv e) => HasRunEnv e where
  runEnv = daemonEnv . deRunEnv
instance {-# OVERLAPS #-} (HasDaemonEnv e) => HasKeySink e where
  keySink = daemonEnv . deKeySink
instance {-# OVERLAPS #-} (HasDaemonEnv e) => Di.HasDispatch e where
  dispatch = daemonEnv . deDispatch
instance {-# OVERLAPS #-} (HasDaemonEnv e) => HasKeymap e where
  keymap = daemonEnv . deKeymap


--------------------------------------------------------------------------------

instance HasDaemonEnv e => HasDaemonEnv (e, a) where
  daemonEnv = _1 . daemonEnv
instance HasButtonEnv e => HasButtonEnv (a, e) where
  buttonEnv = _2 . buttonEnv

instance (HasDaemonEnv e, HasButtonEnv e) => MonadButton (RIO e) where
  emit ka     = view keySink >>= flip emitKeyWith ka
  pause       = threadDelay . (*1000) . fromIntegral
  race ts f   = uncurry A.race ts >>= f
  hold        = Di.pause
  fork        = async
  await       = Di.await
  myBinding   = view binding

runButtonEnv :: HasDaemonEnv e
  => ButtonEnv
  -> RIO (e, ButtonEnv) a
  -> RIO e a
runButtonEnv b = withReader (, b)

--------------------------------------------------------------------------------


-- | Turn a RIO action that requires a 'DaemonEnv' into one that only requires a
-- 'RunEnv' by instantiating a 'DaemonEnv' using the 'DaemonCfg' and running the
-- provided action inside it.
runDaemon :: HasRunEnv e => DaemonCfg -> RIO DaemonEnv a -> RIO e a
runDaemon dfg rio = do
  rnv <- view runEnv
  flip runContT id $ do

    lift $ logInfo "Constructing components"

    snk <- using $ dfg^.keySinkDev
    src <- using $ dfg^.keySourceDev
    dsp <- Di.startDispatch src
    kym <- lift $ newMVar =<< initKeymap (dfg^.keymapCfg)

    let env = DaemonEnv
          { _deDispatch   = dsp
          , _deKeymap     = kym
          , _deDaemonCfg  = dfg
          , _deKeySink    = snk
          , _deRunEnv     = rnv
          }
    pure $ runRIO env rio

startDaemon :: HasDaemonEnv e => RIO e ()
startDaemon = logInfo "Starting app-loop" >> step


step :: HasDaemonEnv e => RIO e ()
step = do
  e <- Di.awaitEvent
  case e of
    KeyIOEvent a -> do
      logDebug $ "Handling key event: " <> pprintDisp a
      lookupKey (a^.thing) >>= \case
        Nothing -> pure ()
        Just benv  -> runButtonEnv benv $ do
          runAction $ benv^.pressAction
          intercept =<< catchRelease (a^.keycode)
                        (runAction =<< releaseButton benv)
      step
    Quit       -> pure ()
    _          -> undefined
