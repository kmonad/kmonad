{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module KMonad.Daemon

where

import Prelude

import qualified UnliftIO.Async as A

import KMonad.Button
import KMonad.Components.Dispatch
import KMonad.Components.KeyHandler
import KMonad.Event
import KMonad.Keyboard
import KMonad.Keyboard.IO
import KMonad.Util
import KMonad.Runner

--------------------------------------------------------------------------------
-- $env

-- | All the configuration options for running a daemon
data DaemonCfg = DaemonCfg
  { _keySinkDev   :: Acquire KeySink
  , _keySourceDev :: Acquire KeySource
  , _keymap       :: Keymap Button
  , _port         :: ()
  }
makeClassy ''DaemonCfg

-- | The running environment of the Daemon
data DaemonEnv = DaemonEnv
  { _deDispatch :: Dispatch
  , _deKeyHandler    :: KeyHandler
  , _deDaemonCfg     :: DaemonCfg
  , _deKeySink       :: KeySink
  , _deRunEnv        :: RunEnv
  }
makeLenses ''DaemonEnv

-- | RIO constraints for being able to perform daemon actions
class ( HasLogFunc e , HasDispatch e , HasKeyHandler e
      , HasKeySink e , HasRunEnv e ) => HasDaemonEnv e where
  daemonEnv :: Lens' e DaemonEnv

instance HasDaemonEnv DaemonEnv where daemonEnv = id

instance {-# OVERLAPS #-} (HasDaemonEnv e) => HasDaemonCfg e where
  daemonCfg = daemonEnv . deDaemonCfg
instance {-# OVERLAPS #-} (HasDaemonEnv e) => HasRunEnv e where
  runEnv = daemonEnv . deRunEnv
instance {-# OVERLAPS #-} (HasDaemonEnv e) => HasKeySink e where
  keySink = daemonEnv . deKeySink
instance {-# OVERLAPS #-} (HasDaemonEnv e) => HasDispatch e where
  inputDispatch = daemonEnv . deDispatch
instance {-# OVERLAPS #-} (HasDaemonEnv e) => HasKeyHandler e where
  keyHandler = daemonEnv . deKeyHandler

instance HasDaemonEnv e => HasDaemonEnv (e, a) where
  daemonEnv = _1 . daemonEnv
instance HasBinding (a, Keycode) where
  binding = _2

--------------------------------------------------------------------------------
-- $mbut

instance (HasDaemonEnv e) => MonadButton (RIO (e, Keycode)) where
  emit ka   = view keySink >>= flip emitKeyWith ka
  pause     = threadDelay . (*1000) . fromIntegral
  race ts f = uncurry A.race ts >>= f
  hold      = undefined
  fork      = async
  waitFor   = undefined
  boundTo   = undefined

--------------------------------------------------------------------------------

runDaemon :: HasRunEnv e => DaemonCfg -> RIO DaemonEnv a -> RIO e a
runDaemon dcfg rio = do
  renv <- view runEnv

  with (liftA2 (,) (dcfg^.keySinkDev) (dcfg^.keySourceDev)) $ \(snk, src) -> do

    logInfo "Constructing components"
    dispatch <- mkDispatch src
    keyhdlr  <- mkKeyHandler $ dcfg^.keymap

    let env = DaemonEnv
          { _deDispatch = dispatch
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
  case e of
    KIOEvent a -> do
      logDebug $ "Handling key event: " <> pprintDisp a
      act <- handleKey (a^.thing)
      withReader (, a^.keycode) $ runAction act
      step
    Quit       -> pure ()
    _          -> undefined

-- runActi

-- runAction :: HasDaemonEnv e => Action -> RIO e ()
-- runAction (Emit a) = view keySink >>= \snk -> emitKeyWith snk a
-- runAction Pass     = pure ()
-- runAction _ = undefined
