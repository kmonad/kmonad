module KMonad.Engine

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
--


data DaemonCfg = DaemonCfg
  { _keySink   :: Acquire KeySink
  , _keySource :: Acquire KeySource
  , _keymap    :: Keymap ButtonCfg
  , _logFunc   :: LogFunc
  , _port      :: ()
  }
makeClassy ''DaemonCfg

data DaemonEnv = DaemonEnv
  { _deInputDispatch :: InputDispatch
  , _deKeyHandler    :: KeyHandler
  , _deDaemonCfg     :: DaemonCfg
  }
makeLenses ''DaemonEnv

instance HasInputDispatch DaemonEnv where inputDispatch = deInputDispatch
instance HasKeyHandler    DaemonEnv where keyHandler    = deKeyHandler
instance HasLogFunc       DaemonEnv where logFuncL      = logFunc
instance HasDaemonCfg     DaemonEnv where daemonCfg     = deDaemonCfg

class (HasLogFunc e, HasInputDispatch e, HasKeyHandler e) => HasDaemonEnv e where
  daemonEnv :: Lens' e DaemonEnv
instance HasDaemonEnv DaemonEnv where daemonEnv = id

startDaemon :: (HasLogFunc e) => DaemonCfg -> RIO e ()
startDaemon cfg =
  with (liftA2 (,) (cfg^.keySink) (cfg^.keySource)) $ \(_, src) -> do
    dispatch <- mkInputDispatch src
    keyhdlr  <- mkKeyHandler $ cfg^.keymap

    let env = DaemonEnv
          { _deInputDispatch = dispatch
          , _deKeyHandler    = keyhdlr
          , _deDaemonCfg     = cfg
          }
    runRIO env step

step :: HasDaemonEnv e => RIO e ()
step = do
  e <- awaitEvent
  case e of
    KIOEvent a -> handleKey (a^.thing) >> step
    Quit       -> pure ()
    _          -> undefined
