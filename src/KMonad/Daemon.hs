{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module KMonad.Daemon
  ( DaemonCfg(..)
  , runDaemon
  , loop
  , Keymap
  )
where

import KPrelude

import KMonad.Button
import KMonad.Event
import KMonad.Keyboard
import KMonad.Keyboard.IO
import KMonad.Util
import KMonad.Runner

import KMonad.Daemon.KeyHandler

import qualified KMonad.Daemon.Dispatch     as Di
import qualified KMonad.Daemon.HookStore    as Hs
import qualified KMonad.Daemon.InjectPoint  as Ip
import qualified KMonad.Daemon.Sluice       as Sl


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
data Daemon = Daemon
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
  , _deKeyHandler  :: KeyHandler
  } 
makeLenses ''Daemon

class ( HasLogFunc         e
      , HasKeySink         e
      , HasKeySource       e
      , HasRunEnv          e
      , Di.HasDispatch     e
      , Hs.HasHookStore    e
      , Sl.HasSluice       e
      , Ip.HasInjectPoint  e
      , HasKeyHandler   e
      )
  => HasDaemon e where
  daemon :: Lens' e Daemon

instance HasDaemon Daemon where daemon = id

instance {-# OVERLAPS #-} (HasDaemon e) => HasDaemonCfg e where
  daemonCfg = daemon . deDaemonCfg
instance {-# OVERLAPS #-} (HasDaemon e) => HasRunEnv e where
  runEnv = daemon . deRunEnv
instance {-# OVERLAPS #-} (HasDaemon e) => HasKeySink e where
  keySink = daemon . deKeySink
instance {-# OVERLAPS #-} (HasDaemon e) => HasKeySource e where
  keySource = daemon . deKeySource
instance {-# OVERLAPS #-} (HasDaemon e) => Di.HasDispatch e where
  dispatch = daemon . deDispatch
instance {-# OVERLAPS #-} (HasDaemon e) => Hs.HasHookStore e where
  hookStore = daemon . deHookStore
instance {-# OVERLAPS #-} (HasDaemon e) => Sl.HasSluice e where
  sluice = daemon . deSluice
instance {-# OVERLAPS #-} (HasDaemon e) => Ip.HasInjectPoint e where
  injectPoint = daemon . deInjectPoint
instance {-# OVERLAPS #-} (HasDaemon e) => HasKeyHandler e where
  keyHandler = daemon . deKeyHandler

--------------------------------------------------------------------------------
-- $init
--

-- | Initialize all the components of the Daemon
mkDaemon :: HasRunEnv e => DaemonCfg -> ContT r (RIO e) Daemon
mkDaemon cfg = do
  -- Get a reference to the RunEnv
  rnv <- view runEnv

  -- Acquire the OS KeyIO actions
  snk <- using $ cfg^.keySinkDev
  src <- using $ cfg^.keySourceDev

  -- Initialize the components, hook them up so they pull from eachother
  dsp <- Di.mkDispatch   
  hks <- Hs.mkHookStore
  slc <- Sl.mkSluice
  ijp <- Ip.mkInjectPoint

  -- Initialize components that are not part of the pull-chain
  kyh <- mkKeyHandler  $ cfg^.keymapCfg

  -- Construct and return the Daemon
  pure $ Daemon
    { _deDaemonCfg   = cfg
    , _deRunEnv      = rnv
    , _deKeySink     = snk
    , _deKeySource   = src
    , _deDispatch    = dsp
    , _deHookStore   = hks
    , _deInjectPoint = ijp
    , _deSluice      = slc
    , _deKeyHandler  = kyh
    }

-- | Reduce a RIO action that requires a 'Daemon' to one that requires only a
-- 'RunEnv'. If you are in a 'RunEnv', this simply runs the 'Daemon' action.
runDaemon :: HasRunEnv e => DaemonCfg -> RIO Daemon a -> RIO e a
runDaemon cfg a = runContT (mkDaemon cfg) $ flip runRIO a


--------------------------------------------------------------------------------
-- $mbut

data KEnv = KEnv
  { _kDaemon    :: Daemon
  , _kButtonEnv :: ButtonEnv
  }
makeLenses ''KEnv

class (HasDaemon e, HasButtonEnv e) => HasKEnv e where
  kEnv :: Lens' e KEnv
instance HasKEnv KEnv where kEnv = id

instance {-# OVERLAPS #-} (HasKEnv e) => HasDaemon e where
  daemon = kEnv . kDaemon
instance {-# OVERLAPS #-} (HasKEnv e) => HasButtonEnv e where
  buttonEnv = kEnv . kButtonEnv


-- | When 'True', set the 'Sluice' to blocked mode, when 'False', unblock the
-- 'Sluice' and rerun all the Events.
kHold :: HasDaemon e => Bool -> RIO e ()
kHold = bool (Sl.unblock >>= Di.rerun) Sl.block

instance HasKEnv e => MonadButton (RIO e) where
  emit        = emitKey
  pause       = threadDelay . (*1000) . fromIntegral
  hold        = kHold
  myBinding   = view binding
  hookNext    = Hs.hookNext
  hookWithin  = Hs.hookWithin

runK :: HasDaemon e => ButtonEnv -> RIO KEnv a -> RIO e a
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
  runButton Press >>= maybe (pure ()) (\a -> do
      runAction a
      matchRelease_ releaseButton)

-- | Release the current 'Button'
--
-- This:
--   1. Executes the 'releaseAction' of the currently active Button
--   2. Sets the 'lastAction' to 'Release'.
releaseButton :: RIO KEnv ()
releaseButton = do
  runButton Release >>= maybe (pure ()) runAction


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
nextEvent :: HasDaemon e => RIO e Event
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
handleKey e = lookupKey (e^.keyAction) >>= \case
  Nothing -> pure ()
  Just b  -> runK b $ pressButton

-- | Handle a message by crashing instantly. TODO: This might need to be different.
handleMsg :: MsgEvent -> RIO e ()
handleMsg = undefined
