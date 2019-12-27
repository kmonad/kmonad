module KMonad.Daemon

where

import KMonad.Prelude

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
  , _logFunc    :: LogFunc
  , _port       :: ()
  }
makeClassy ''DaemonCfg

instance HasLogFunc DaemonCfg where logFuncL = logFunc

data DaemonEnv = DaemonEnv
  { _deInputDispatch :: InputDispatch
  , _deKeyHandler    :: KeyHandler
  , _deDaemonCfg     :: DaemonCfg
  , _deKeySink       :: KeySink
  }
makeLenses ''DaemonEnv

instance HasInputDispatch DaemonEnv where inputDispatch = deInputDispatch
instance HasKeyHandler    DaemonEnv where keyHandler    = deKeyHandler
instance HasLogFunc       DaemonEnv where logFuncL      = logFunc
instance HasDaemonCfg     DaemonEnv where daemonCfg     = deDaemonCfg
instance HasKeySink       DaemonEnv where keySink       = deKeySink

class ( HasLogFunc e, HasInputDispatch e, HasKeyHandler e
      , HasKeySink e ) => HasDaemonEnv e where
  daemonEnv :: Lens' e DaemonEnv
instance HasDaemonEnv DaemonEnv where daemonEnv = id

startDaemon :: (HasDaemonCfg e, HasLogFunc e) => RIO e ()
startDaemon = view daemonCfg >>= \cfg -> do
  logInfo "Acquiring Key IO"
  with (liftA2 (,) (cfg^.keySinkA) (cfg^.keySourceA)) $ \(snk, src) -> do
    logInfo "Constructing components"
    dispatch <- mkInputDispatch src
    keyhdlr  <- mkKeyHandler $ cfg^.keymap

    let env = DaemonEnv
          { _deInputDispatch = dispatch
          , _deKeyHandler    = keyhdlr
          , _deDaemonCfg     = cfg
          , _deKeySink       = snk
          }
    logInfo "Starting app-loop"
    runRIO env step

step :: HasDaemonEnv e => RIO e ()
step = do
  e <- awaitEvent
  logInfo $ "Received " <> displayShow e
  case e of
    KIOEvent a -> do
      act <- handleKey (a^.thing)
      runAction act
      displayKeyHandler
      step
    Quit       -> pure ()
    _          -> undefined

runAction :: HasDaemonEnv e => Action -> RIO e ()
runAction (Emit a) = logInfo "hello" >> emitKey a
runAction Pass     = pure ()
runAction _ = undefined
