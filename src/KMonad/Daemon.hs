module KMonad.Daemon

where

import Prelude

import KMonad.Button
import KMonad.Components.InputDispatch
import KMonad.Components.KeyHandler
import KMonad.Event
import KMonad.Keyboard.IO
import KMonad.Util

--------------------------------------------------------------------------------
-- $env


data DaemonCfg = DaemonCfg
  { _keySinkA   :: Acquire KeySink
  , _keySourceA :: Acquire KeySource
  , _keymap     :: Keymap ButtonCfg
  , _port       :: ()
  }
makeClassy ''DaemonCfg

data DaemonEnv = DaemonEnv
  { _deInputDispatch :: InputDispatch
  , _deKeyHandler    :: KeyHandler
  , _deDaemonCfg     :: DaemonCfg
  , _deKeySink       :: KeySink
  , _deRunEnv        :: RunEnv
  }
makeLenses ''DaemonEnv

instance HasInputDispatch DaemonEnv where inputDispatch = deInputDispatch
instance HasKeyHandler    DaemonEnv where keyHandler    = deKeyHandler
instance HasLogFunc       DaemonEnv where logFuncL      = runEnv.logFuncL
instance HasDaemonCfg     DaemonEnv where daemonCfg     = deDaemonCfg
instance HasRunEnv        DaemonEnv where runEnv        = deRunEnv
instance HasKeySink       DaemonEnv where keySink       = deKeySink

class ( HasLogFunc e, HasInputDispatch e, HasKeyHandler e
      , HasKeySink e, HasRunEnv e) => HasDaemonEnv e where
  daemonEnv :: Lens' e DaemonEnv
instance HasDaemonEnv DaemonEnv where daemonEnv = id

startDaemon :: HasRunEnv e => DaemonCfg -> RIO e ()
startDaemon dcfg = do
  renv <- view runEnv
  logInfo "Acquiring Key IO"
  with (liftA2 (,) (dcfg^.keySinkA) (dcfg^.keySourceA)) $ \(snk, src) -> do
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
    logInfo "Starting app-loop"
    runRIO env step

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
