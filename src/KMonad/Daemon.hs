{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module KMonad.Daemon
  -- ( DaemonCfg(..)
  -- , runDaemon
  -- , startDaemon
  -- )
where

import Prelude

import Control.Monad.Except
import GHC.Conc (orElse)
import Data.Semigroup (Any(..))

import KMonad.Button
import KMonad.Event
import KMonad.Keyboard
import KMonad.Keyboard.IO
import KMonad.Util
import KMonad.Runner

import qualified UnliftIO.Async           as A
import qualified RIO.Seq                  as Q


--------------------------------------------------------------------------------
-- $env
--

-- | All the configuration options for running a daemon
data DaemonCfg = DaemonCfg
  { _keySinkDev   :: Acquire KeySink
  , _keySourceDev :: Acquire KeySource
  , _keymapCfg    :: Keymap Button
  , _port         :: ()
  }
makeClassy ''DaemonCfg

-- | The runtime environment of the daemon
data DaemonEnv = DaemonEnv
  { -- KeyEvent input handling

    -- Buttonmap
  , _buttonMap    :: TVar (Keymap ButtonEnv)
  }
makeLenses ''DaemonEnv

-- | RIO constraints for being able to perform daemon actions
class ( HasLogFunc e, HasKeymap e, HasKeySink e , HasRunEnv e )
  => HasDaemonEnv e where
  daemonEnv :: Lens' e DaemonEnv

instance HasDaemonEnv DaemonEnv where daemonEnv = id

instance {-# OVERLAPS #-} (HasDaemonEnv e) => HasDaemonCfg e where
  daemonCfg = daemonEnv . deDaemonCfg
instance {-# OVERLAPS #-} (HasDaemonEnv e) => HasRunEnv e where
  runEnv = daemonEnv . deRunEnv
instance {-# OVERLAPS #-} (HasDaemonEnv e) => HasKeySink e where
  keySink = daemonEnv . deKeySink
instance {-# OVERLAPS #-} (HasDaemonEnv e) => HasKeymap e where
  keymap = daemonEnv . deKeymap


--------------------------------------------------------------------------------
-- $apploop



-- | Return the next 'Event' that reaches the app-loop. This can either be an
-- injected event or an event received from the keyboard. This action will keep
-- retrying getKey and running its IO actions until one succeeds.
nextEvent :: HasDaemonEnv e => RIO e Event
nextEvent = do
  d <- view daemonEnv
  let next = atomically $ (Left  <$> takeTMVar (d^.injectV))
                 `orElse` (Right <$> getKey d)
  next >>= \case
    Left  e             -> pure e
    Right (Nothing, io) -> liftIO io >> nextEvent
    Right (Just e,  io) -> liftIO io >> (pure . KeyIOEvent $ e)


step :: HasDaemonEnv e => RIO e ()
step = do
  e <- nextEvent
  undefined

--------------------------------------------------------------------------------
-- $ops

mbHold :: HasDaemonEnv e => Bool -> RIO e ()
mbHold = undefined

mbAwait :: HasDaemonEnv e => (KeyEvent -> Bool) -> RIO e KeyEvent
mbAwait = undefined



--------------------------------------------------------------------------------
-- $mbut

instance HasDaemonEnv e => HasDaemonEnv (e, a) where
  daemonEnv = _1 . daemonEnv
instance HasButtonEnv e => HasButtonEnv (a, e) where
  buttonEnv = _2 . buttonEnv

instance (HasDaemonEnv e, HasButtonEnv e) => MonadButton (RIO e) where
  emit ka     = view keySink >>= flip emitKeyWith ka
  pause       = threadDelay . (*1000) . fromIntegral
  hold        = mbHold
  myBinding   = view binding
  catchNext   = mbCatchNext
  catchWithin = mbCatchWithin

-- runButtonEnv :: HasDaemonEnv e
--   => ButtonEnv
--   -> RIO (e, ButtonEnv) a
--   -> RIO e a
-- runButtonEnv b = withReader (, b)


-- -- | The running environment of the Daemon
-- data DaemonEnv = DaemonEnv
--   { _deDispatch   :: Di.Dispatch
--   , _deKeymap     :: MVar (Keymap ButtonEnv)
--   , _deDaemonCfg  :: DaemonCfg
--   , _deKeySink    :: KeySink
--   , _deRunEnv     :: RunEnv
--   }
-- makeLenses ''DaemonEnv




-- --------------------------------------------------------------------------------
-- -- $ops


-- -- | Turn a RIO action that requires a 'DaemonEnv' into one that only requires a
-- -- 'RunEnv' by instantiating a 'DaemonEnv' using the 'DaemonCfg' and running the
-- -- provided action inside it.
-- runDaemon :: HasRunEnv e => DaemonCfg -> RIO DaemonEnv a -> RIO e a
-- runDaemon dfg rio = do
--   rnv <- view runEnv
--   flip runContT id $ do

--     lift $ logInfo "Constructing components"

--     snk <- using $ dfg^.keySinkDev
--     src <- using $ dfg^.keySourceDev
--     dsp <- Di.startDispatch src
--     kym <- lift $ newMVar =<< initKeymap (dfg^.keymapCfg)

--     let env = DaemonEnv
--           { _deDispatch   = dsp
--           , _deKeymap     = kym
--           , _deDaemonCfg  = dfg
--           , _deKeySink    = snk
--           , _deRunEnv     = rnv
--           }
--     pure $ runRIO env rio

-- startDaemon :: HasDaemonEnv e => RIO e ()
-- startDaemon = logInfo "Starting app-loop" >> step

-- step :: HasDaemonEnv e => RIO e ()
-- step = do
--   e <- Di.awaitEvent
--   case e of
--     KeyIOEvent a -> do
--       logDebug $ "Handling key event: " <> pprintDisp a
--       lookupKey (a^.thing) >>= \case
--         Nothing -> pure ()
--         Just benv  -> runButtonEnv benv $ do
--           runAction $ benv^.pressAction
--           intercept =<< catchRelease (a^.keycode)
--                         (runAction =<< releaseButton benv)
--       step
--     Quit       -> pure ()
--     _          -> undefined
