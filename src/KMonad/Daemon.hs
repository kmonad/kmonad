{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module KMonad.Daemon

where

import Prelude

import KMonad.Button
import KMonad.Components.InputDispatch
import KMonad.Components.KeyHandler
import KMonad.Event
import KMonad.Keyboard.IO
import KMonad.Util
import KMonad.Runner

--------------------------------------------------------------------------------
-- $env

-- | All the configuration options for running a daemon
data DaemonCfg = DaemonCfg
  { _keySinkDev   :: Acquire KeySink
  , _keySourceDev :: Acquire KeySource
  , _keymap       :: Keymap ButtonCfg
  , _port         :: ()
  }
makeClassy ''DaemonCfg

-- | The running environment of the Daemon
data DaemonEnv = DaemonEnv
  { _deInputDispatch :: InputDispatch
  , _deKeyHandler    :: KeyHandler
  , _deDaemonCfg     :: DaemonCfg
  , _deKeySink       :: KeySink
  , _deRunEnv        :: RunEnv
  }
makeLenses ''DaemonEnv

-- | RIO constraints for being able to perform daemon actions
class ( HasLogFunc e , HasInputDispatch e , HasKeyHandler e
      , HasKeySink e , HasRunEnv e ) => HasDaemonEnv e where
  daemonEnv :: Lens' e DaemonEnv

instance HasDaemonEnv DaemonEnv where daemonEnv = id

instance {-# OVERLAPS #-} (HasDaemonEnv e) => HasDaemonCfg e where
  daemonCfg = daemonEnv . deDaemonCfg
instance {-# OVERLAPS #-} (HasDaemonEnv e) => HasRunEnv e where
  runEnv = daemonEnv . deRunEnv
instance {-# OVERLAPS #-} (HasDaemonEnv e) => HasKeySink e where
  keySink = daemonEnv . deKeySink
instance {-# OVERLAPS #-} (HasDaemonEnv e) => HasInputDispatch e where
  inputDispatch = daemonEnv . deInputDispatch
instance {-# OVERLAPS #-} (HasDaemonEnv e) => HasKeyHandler e where
  keyHandler = daemonEnv . deKeyHandler


--------------------------------------------------------------------------------

runDaemon :: HasRunEnv e => DaemonCfg -> RIO DaemonEnv a -> RIO e a
runDaemon dcfg rio = do
  renv <- view runEnv

  logInfo "Acquiring Key IO"
  with (liftA2 (,) (dcfg^.keySinkDev) (dcfg^.keySourceDev)) $ \(snk, src) -> do

    logInfo "Constructing components"
    dispatch <- mkInputDispatch src
    keyhdlr  <- mkKeyHandler $ dcfg^.keymap

    let env = DaemonEnv
          { _deInputDispatch = dispatch
          , _deKeyHandler    = keyhdlr
          , _deDaemonCfg     = dcfg
          , _deKeySink       = snk
          , _deRunEnv        = renv
          }
    runRIO env rio


startDaemon :: HasDaemonEnv e => RIO e ()
startDaemon = logInfo "Starting app-loop" >> step

step :: HasDaemonEnv e => RIO e ()
step = do
  e <- awaitEvent
  logDebug $ "Received " <> displayShow e
  case e of
    KIOEvent a -> do
      act <- handleKey (a^.thing)
      runAction act
      -- displayKeyHandler
      step
    Quit       -> pure ()
    _          -> undefined

runAction :: HasDaemonEnv e => Action -> RIO e ()
runAction (Emit a) = view keySink >>= \snk -> emitKeyWith snk a
runAction Pass     = pure ()
runAction _ = undefined
