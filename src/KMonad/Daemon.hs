{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module KMonad.Daemon

where

import Prelude


import KMonad.Button
import KMonad.Event
import KMonad.Keyboard
import KMonad.Keyboard.IO
import KMonad.Util
import KMonad.Runner


import qualified UnliftIO.Async as A
import qualified KMonad.Components.Dispatch   as D
import qualified KMonad.Components.KeyHandler as K

--------------------------------------------------------------------------------
-- $env

-- | All the configuration options for running a daemon
data DaemonCfg = DaemonCfg
  { _keySinkDev   :: Acquire KeySink
  , _keySourceDev :: Acquire KeySource
  , _keymap       :: K.Keymap Button
  , _port         :: ()
  }
makeClassy ''DaemonCfg

-- | The running environment of the Daemon
data DaemonEnv = DaemonEnv
  { _deDispatch   :: D.Dispatch
  , _deKeyHandler :: K.KeyHandler
  , _deDaemonCfg  :: DaemonCfg
  , _deKeySink    :: KeySink
  , _deRunEnv     :: RunEnv
  }
makeLenses ''DaemonEnv

-- | RIO constraints for being able to perform daemon actions
class ( HasLogFunc e , D.HasDispatch e , K.HasKeyHandler e
      , HasKeySink e , HasRunEnv e ) => HasDaemonEnv e where
  daemonEnv :: Lens' e DaemonEnv

instance HasDaemonEnv DaemonEnv where daemonEnv = id

instance {-# OVERLAPS #-} (HasDaemonEnv e) => HasDaemonCfg e where
  daemonCfg = daemonEnv . deDaemonCfg
instance {-# OVERLAPS #-} (HasDaemonEnv e) => HasRunEnv e where
  runEnv = daemonEnv . deRunEnv
instance {-# OVERLAPS #-} (HasDaemonEnv e) => HasKeySink e where
  keySink = daemonEnv . deKeySink
instance {-# OVERLAPS #-} (HasDaemonEnv e) => D.HasDispatch e where
  dispatch = daemonEnv . deDispatch
instance {-# OVERLAPS #-} (HasDaemonEnv e) => K.HasKeyHandler e where
  keyHandler = daemonEnv . deKeyHandler


--------------------------------------------------------------------------------
-- $mbut
--
-- We satisfy all of the different capabilities of MonadButton using the
-- different components of the daemon. The only additional thing we have to pass
-- in is the keycode of the currently processing button (which can't be part of
-- the DaemonEnv, because sometimes we aren't processing a button).

instance HasDaemonEnv e => HasDaemonEnv (e, a) where daemonEnv = _1 . daemonEnv

instance HasDaemonEnv e => MonadButton (RIO (e, Keycode)) where
  emit ka     = view keySink >>= flip emitKeyWith ka
  pause       = threadDelay . (*1000) . fromIntegral
  race ts f   = uncurry A.race ts >>= f
  hold        = D.pauseStream
  fork        = async
  await       = D.intercept
  myBinding   = view _2


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
    dsp <- D.withDispatch src
    kyh <- lift $ K.mkKeyHandler (dfg^.keymap)

    let env = DaemonEnv
          { _deDispatch   = dsp
          , _deKeyHandler = kyh
          , _deDaemonCfg  = dfg
          , _deKeySink    = snk
          , _deRunEnv     = rnv
          }
    pure $ runRIO env rio

startDaemon :: HasDaemonEnv e => RIO e ()
startDaemon = logInfo "Starting app-loop" >> step

step :: HasDaemonEnv e => RIO e ()
step = do
  e <- D.awaitEvent
  case e of
    KIOEvent a -> do
      logDebug $ "Handling key event: " <> pprintDisp a
      act <- K.handleKey (a^.thing)
      withReader (, a^.keycode) $ runAction act
      step
    Quit       -> pure ()
    _          -> undefined
