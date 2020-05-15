module KMonad.Daemon
  ( DaemonCfg(..)
  , runDaemon
  , loop
  , Kh.Keymap
  )
where

import KPrelude

import KMonad.Button
import KMonad.Event
import KMonad.Keyboard
import KMonad.Keyboard.IO
import KMonad.Util
import KMonad.Runner.Types


import qualified KMonad.Daemon.Dispatch     as Di
import qualified KMonad.Daemon.HookStore    as Hs
import qualified KMonad.Daemon.KeyHandler   as Kh
import qualified KMonad.Daemon.InjectPoint  as Ip
import qualified KMonad.Daemon.Sluice       as Sl
import qualified KMonad.Daemon.Server       as Sv


--------------------------------------------------------------------------------
-- $env
--

-- | All the configuration options for running a daemon
data DaemonCfg = DaemonCfg
  { _keySinkDev   :: Acquire KeySink
  , _keySourceDev :: Acquire KeySource
  , _keymapCfg    :: Kh.Keymap Button
  , _firstLayer   :: LayerTag
  , _port         :: Sv.Port
  }
makeClassy ''DaemonCfg

-- | The runtime environment of the daemon
data DaemonEnv = DaemonEnv
  { -- Other configurations
    _deDaemonCfg   :: DaemonCfg
  , _deRunEnv      :: RunEnv

    -- KeyEvent input handling
  , _deKeySink     :: KeySink
  , _deKeySource   :: KeySource

    -- Components
  , _deDispatch    :: Di.Dispatch
  , _deHookStore   :: Hs.HookStore
  , _deInjectPoint :: Ip.InjectPoint
  , _deSluice      :: Sl.Sluice
  , _deKeyHandler  :: Kh.KeyHandler
  , _deServer      :: Sv.Server
  } 
makeClassy ''DaemonEnv

-- class ( HasLogFunc         e
--       , HasKeySink         e
--       , HasKeySource       e
--       , HasRunEnv          e
--       , Di.HasDispatch     e
--       , Hs.HasHookStore    e
--       , Sl.HasSluice       e
--       , Ip.HasInjectPoint  e
--       , Kh.HasKeyHandler   e
--       , Sv.HasServer       e
--       )
--   => HasDaemon e where
--   daemon :: Lens' e Daemon

-- instance HasDaemon Daemon where daemon = id

--------------------------------------------------------------------------------
-- $init
--

-- | Initialize all the components of the Daemon
mkDaemon :: HasRunEnv e => DaemonCfg -> ContT r (RIO e) DaemonEnv
mkDaemon cfg = do
  -- Get a reference to the RunEnv
  rnv <- view runEnv

  -- Acquire the OS KeyIO actions
  snk <- using $ cfg^.keySinkDev
  src <- using $ cfg^.keySourceDev

  -- Initialize the components of the pull-chain
  dsp <- Di.mkDispatch   
  hks <- Hs.mkHookStore
  slc <- Sl.mkSluice
  ijp <- Ip.mkInjectPoint

  -- Initialize components that are not part of the pull-chain
  kyh <- Kh.mkKeyHandler (cfg^.firstLayer) (cfg^.keymapCfg)
  srv <- Sv.mkServer (cfg^.port)

  -- Construct and return the Daemon
  pure $ DaemonEnv
    { _deDaemonCfg   = cfg
    , _deRunEnv      = rnv
    , _deKeySink     = snk
    , _deKeySource   = src
    , _deDispatch    = dsp
    , _deHookStore   = hks
    , _deInjectPoint = ijp
    , _deSluice      = slc
    , _deKeyHandler  = kyh
    , _deServer      = srv
    }

-- initDaemon :: ContT r (RIO Daemon) ()
-- initDaemon = launch_ "msg-inject" (MessageEvent <$> Sv.recvMsg >>= Ip.inject)

initDaemon :: (HasLogFunc e, Sv.HasServer e, Ip.HasInjectPoint e) => ContT r (RIO e) ()
initDaemon = launch_ "msg-inject" (MessageEvent <$> Sv.recvMsg >>= Ip.inject)

-- | Reduce a RIO action that requires a 'DaemonEnv' to one that requires only a
-- 'RunEnv'. If you are in a 'RunEnv', this simply runs the 'Daemon' action.
runDaemon :: HasRunEnv e => DaemonCfg -> RIO DaemonEnv a -> RIO e a
runDaemon cfg a = do
  dem <- runContT (mkDaemon cfg) pure

  withLaunch_ "msg-inject" (MessageEvent <$> Sv.recvMsg >>= Ip.inject) $
    runRIO dem a


  -- runContT (mkDaemon cfg) $ \dm -> runRIO dm a
  undefined

  -- flip runContT (flip runRIO (startServer >> a)) $ do
  -- dm <- mkDaemon cfg

  -- dm <- mkDaemon cfg
  -- runRIO dm a
  -- launch_ "msg-inject" (MessageEvent <$> Sv.recvMsg >>= Ip.inject)
  
-- runContT (mkDaemon cfg) $ flip runRIO a
--------------------------------------------------------------------------------
-- $mbut

data KEnv = KEnv
  { _kDaemonEnv :: DaemonEnv
  , _kButtonEnv :: Kh.ButtonEnv
  }
makeLenses ''KEnv

instance HasDaemonEnv KEnv where
  daemonEnv = kDaemonEnv
instance Kh.HasButtonEnv KEnv where
  buttonEnv = kButtonEnv


-- | When 'True', set the 'Sluice' to blocked mode, when 'False', unblock the
-- 'Sluice' and rerun all the KeyEvents.
kHold :: HasDaemonEnv e => Bool -> RIO e ()
kHold = bool (Sl.unblock >>= Di.rerun) Sl.block

instance HasKEnv e => MonadK (RIO e) where
  emit        = emitKey
  pause       = threadDelay . (*1000) . fromIntegral
  hold        = kHold
  myBinding   = view (Kh.binding)
  hookNext    = Hs.hookNext
  hookWithin  = Hs.hookWithin
  layerOp     = Kh.layerOp

runK :: HasDaemon e => Kh.ButtonEnv -> RIO KEnv a -> RIO e a
runK b a = view daemon >>= \d -> runRIO (KEnv d b) a

-- | Press the current 'Button'
--
-- This:
--   1. Executes the 'pressAction' of the currently active Button
--   2. Sets 'lastAction' to 'Press', ensuring further presses do nothing until
--      a 'releaseButton' action has succesfully completed.
--   3. Adds a hook to the release of this 'Button' which will execute
--      'releaseButton'
pressButton :: RIO KEnv ()
pressButton = do
  Kh.runButton Press >>= maybe (pure ()) (\a -> do
      runAction a
      matchRelease_ releaseButton)

-- | Release the current 'Button'
--
-- This:
--   1. Executes the 'releaseAction' of the currently active Button
--   2. Sets the 'lastAction' to 'Release'.
releaseButton :: RIO KEnv ()
releaseButton = do
  Kh.runButton Release >>= maybe (pure ()) runAction


--------------------------------------------------------------------------------
-- $loop

-- | Wait for the next event to occur.
--
-- NOTE: This is implemented as pulling from the 'InjectPoint'. The components
-- of the Daemon are arranged in such a way that they pull from eachother, so
-- pulling from the 'InjectPoint' will cause the 'InjectPoint' to start pulling
-- from the 'Sluice', which pulls from the 'HookStore', which pulls from the
-- 'Dispatch'. Each of those pulls comes with possible side-effects, like
-- blocking, dispatching to external processes, or event injection.
--
-- The moment a pull succeeds, it stops trying to pull, so when 'nextEvent'
-- returns, we know that the entire pull-chain has stopped. Therefore we will
-- never be handling an event while another event is already in the middle of
-- the pull-chain.
nextEvent :: HasDaemon e => RIO e KeyEvent
nextEvent = Ip.pull . Sl.pull . Hs.pull . Di.pull $ awaitKey

-- | Fetch the next event from the pull-chain of components and subsequently
-- pass it to the appropriate handler (or exit the loop).
loop :: HasDaemon e => RIO e ()
loop = do
  nextEvent >>= \case
    Quit           -> pure ()
    KeyIOEvent   e -> handleKey e >> loop
    MessageEvent e -> handleMsg e >> loop

-- | Handle a key press by running 'pressButton' in its environment, any other
-- key event gets ignored.
handleKey :: HasDaemon e => KeyEvent -> RIO e ()
handleKey e = Kh.lookupKey (e^.keyAction) >>= \case
  Nothing -> pure ()
  Just b  -> runK b $ pressButton

-- | Handle a message by crashing instantly. TODO: This might need to be different.
handleMsg :: MsgEvent -> RIO e ()
handleMsg = undefined
