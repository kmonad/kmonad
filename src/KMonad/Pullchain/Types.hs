module KMonad.Pullchain.Types
  ( LMap
  , KeyPred
  , Catch(..)
  , Trigger(..)
  , Timeout(..)
  , Hook(..)

    -- * Lenses
  , HasHook(..)
  , HasTimeout(..)
  , HasTrigger(..)

    -- * Layer operations
    -- $lop
  , LayerOp(..)

    -- * MonadK
    -- $monadk
  , MonadKIO(..)
  , MonadK(..)
  , AnyK
  , Action(..)

    -- * Button
  , Button(..)
  , HasButton(..)

    -- * BEnv
  , BEnv(..)
  , HasBEnv(..)
  , mkBEnv
  , runBEnv
  )
where

import KMonad.Prelude
import KMonad.Util
import qualified KMonad.Util.LayerStack as Ls
import UnliftIO.Process (CreateProcess(close_fds), createProcess_, shell)
type KeyPred = KeySwitch -> Bool

-- | 'LMap's are mappings from 'Name'd maps from 'Keycode' to things.
type LMap a = Ls.LayerStack Name Keycode a

--------------------------------------------------------------------------------
-- $keyfun

-- | Boolean isomorph signalling wether an event should be caught or not
data Catch = Catch | NoCatch deriving (Show, Eq)

instance Semigroup Catch where
  NoCatch <> NoCatch = NoCatch
  _       <> _       = Catch

instance Monoid Catch where
  mempty = NoCatch

-- | The packet used to trigger a KeyFun, containing info about the event and
-- how long since the Hook was registered.
data Trigger = Trigger
  { _elapsed :: Ms -- ^ Time elapsed since hook was registered
  , _event   :: KeySwitch     -- ^ The key event triggering this call
  }
makeClassy ''Trigger


--------------------------------------------------------------------------------
-- $hook
--
-- The general structure of the 'Hook' record, that defines the most general way
-- of registering a 'KeySwitch' function.

-- | A 'Timeout' value describes how long to wait and what to do upon timeout
data Timeout m = Timeout
  { _delay  :: Ms -- ^ Delay before timeout action is triggered
  , _action :: m ()         -- ^ Action to perform upon timeout
  }
makeClassy ''Timeout

-- | The content for 1 key hook
data Hook m = Hook
  { _hTimeout :: Maybe (Timeout m)  -- ^ Optional timeout machinery
  , _keyH     :: Trigger -> m Catch -- ^ The function to call on the next 'KeySwitch'
  }
makeClassy ''Hook


--------------------------------------------------------------------------------
-- $lop
--
-- Operations that manipulate the layer-stack

-- | 'LayerOp' describes all the different layer-manipulations that KMonad
-- supports.
data LayerOp
  = PushLayer    Name -- ^ Add a layer to the top of the stack
  | PopLayer     Name -- ^ Remove the first occurence of a layer
  | SetBaseLayer Name -- ^ Change the base-layer

--------------------------------------------------------------------------------
-- $monadk
--
-- The fundamental components that make up any 'KMonad.Pullchain.Button.Button' operation.


-- | 'MonadK' contains all the operations used to constitute button actions. It
-- encapsulates all the side-effects required to get everything running.
class Monad m => MonadKIO m where
  -- | Emit a KeySwitch to the OS
  emit       :: KeySwitch -> m ()
  -- | Pause the current thread for n milliseconds
  pause      :: Ms -> m ()
  -- | Pause or unpause event processing
  hold       :: Bool -> m ()
  -- | Register a callback hook
  register   :: Hook m -> m ()
  -- | Run a layer-stack manipulation
  layerOp    :: LayerOp -> m ()
  -- | Insert an event in the input queue
  inject     :: KeySwitch -> m ()
  -- | Run a shell-command
  shellCmd   :: Text -> m ()

-- | 'MonadKIO' contains the additional bindings that get added when we are
-- currently processing a button.
class MonadKIO m => MonadK m where
  -- | Access the keycode to which the current button is bound
  myBinding  :: m Keycode

-- | Type alias for `any monad that can perform MonadK actions`
type AnyK a = forall m. MonadK m => m a

-- | A newtype wrapper used to construct 'MonadK' actions
newtype Action = Action { runAction :: AnyK () }
--------------------------------------------------------------------------------
-- $button

-- | A 'Button' consists of two 'MonadK' actions, one to take when a press is
-- registered from the OS, and another when a release is registered.
data Button = Button
  { _pressAction   :: !Action -- ^ Action to take when pressed
  , _releaseAction :: !Action -- ^ Action to take when released
  }
makeClassy ''Button

--------------------------------------------------------------------------------

-- | The configuration of a 'Button' with some additional state to keep track of
-- the last 'Switch' and the binding to which it has been assigned.
data BEnv = BEnv
  { _beButton   :: !Button        -- ^ The configuration for this button
  , _binding    :: !Keycode       -- ^ The 'Keycode' to which this button is bound
  , _lastSwitch :: !(MVar Switch) -- ^ State to keep track of last manipulation
  }
makeClassy ''BEnv

instance HasButton BEnv where button = beButton

-- | Create a new BEnv
mkBEnv :: MonadIO m => Button -> Keycode -> m BEnv
mkBEnv b c = BEnv b c <$> newMVar Release

-- | Try to switch a 'BEnv'. This only does something if the 'Switch' is
-- different from the 'lastSwitch' field. I.e. pressing a pressed button or
-- releasing a released button does nothing.
--
-- FIXME: This should not live in Types
runBEnv :: MonadUnliftIO m => BEnv -> Switch -> m (Maybe Action)
runBEnv b a =
  modifyMVar (b^.lastSwitch) $ \l -> pure $ case (a, l) of
    (Press, Release) -> (Press,   Just $ b^.pressAction)
    (Release, Press) -> (Release, Just $ b^.releaseAction)
    _                -> (a,       Nothing)

-- --------------------------------------------------------------------------------
-- -- $monadK
-- --
-- -- Overview of all the operations supported in MonadKIO actions

-- instance CanK e => MonadKIO (RIO e) where
--   -- Emitting with the keysink
--   emit e = do
--     ov <- view outVar
--     -- ke <- keyEventNow e
--     atomically $ putTMVar ov e

--   -- Pausing is a simple IO action
--   pause = threadDelay . (*1000) . fromIntegral

--   -- Holding and rerunning through the sluice and dispatch
--   hold b = do
--     sl <- view sluice
--     di <- view dispatch
--     if b then Sl.block sl else Sl.unblock sl >>= Dp.rerun di

--   -- Hooking is performed with the hooks component
--   register l h = do
--     hs <- case l of
--       InputHook  -> view inHooks
--       OutputHook -> view outHooks
--     Hs.register hs h

--   -- Layer-ops are sent to the 'Keymap'
--   layerOp o = view keymap >>= \hl -> Km.layerOp hl o

--   -- Injecting by adding to Dispatch's rerun buffer
--   inject e = do
--     di <- view dispatch
--     -- ke <- keyEventNow e
--     logDebug $ "Injecting event: " <> tshow e
--     Dp.rerun di [e]

--   -- Shell-command through spawnCommand
--   shellCmd t = do
--     f <- view allowCmd
--     if f then do
--       logInfo $ "Running command: " <> tshow t
--       spawnCommand . unpack $ t
--     else
--       logInfo $ "Received but not running: " <> tshow t
--    where
--     spawnCommand :: MonadIO m => String -> m ()
--     spawnCommand cmd = void $ createProcess_ "spawnCommand"
--       (shell cmd){ -- We don't want the child process to inherit things like
--                    -- our keyboard grab (this would, for example, make it
--                    -- impossible for a command to restart kmonad).
--                    close_fds   = True
--                  }
