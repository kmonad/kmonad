module KMonad.Domain.Types

where

import KMonad.Prelude

import Data.Serialize
import Network.Socket

import KMonad.Core
import KMonad.Domain.KeyIO

import qualified RIO.HashMap as M

--------------------------------------------------------------------------------
-- $action
--
-- 'Action's are an AST of different actions that can be performed by 'Button's
-- on their press or release events.


data Action
  = Emit  !KeyAction
  | Pause !Milliseconds
  | Race !(Action, Action) !(Action, Action)
  -- | Hold Bool
  -- | WaitFor (KeyEvent -> Bool)
  | Macro ![Action]
  -- | MaskInput Bool
  | Fork !Action
  | Pass


--------------------------------------------------------------------------------
-- $button
--
-- 'Button's are 2 different 'Action's bound to the press and release events of
-- a particular 'Keycode'. 'Button's are structured so that only
-- state-transitions are effectful (press after press does nothing, release
-- after release does nothing).
--
-- Most button functionality is implemented in the "KMonad.Domain.Button"
-- module, but the type is provided here for circular import reasons.

-- | The configurable aspects of a 'Button'
data ButtonCfg = ButtonCfg
  { _pressAction   :: !Action -- ^ Action to take when pressed
  , _releaseAction :: !Action -- ^ Action to take when released
  }
makeClassy ''ButtonCfg

-- | The environment in which 'Button' actions are run
data ButtonEnv = ButtonEnv
  { _beBinding    :: !Keycode             -- ^ The 'Keycode' to which the button is bound
  , _beLastAction :: !(MVar SwitchAction) -- ^ State to keep track of last manipulation
  , _beButtonCfg  :: !ButtonCfg           -- ^ The configuration for this button
  }
makeLenses ''ButtonEnv

class HasButtonCfg e => HasButtonEnv e where
  buttonEnv :: Lens' e ButtonEnv

  binding :: Lens' e Keycode
  binding = buttonEnv . beBinding

  lastAction :: Lens' e (MVar SwitchAction)
  lastAction = buttonEnv . beLastAction

instance HasButtonCfg ButtonEnv where buttonCfg = beButtonCfg
instance HasButtonEnv ButtonEnv where buttonEnv = id


--------------------------------------------------------------------------------
-- $event
--
-- 'Event's are the collection of all things that can trigger KMonad to update.

-- | 'Event' is the sum of all events that can be given to KMonad
data Event
  = KIOEvent !KeyEvent    -- ^ A 'KeyEvent' is registered from the 'KeySource'
  | MessageEvent !Message -- ^ A 'Message' is received through the network socket
  | Quit                  -- ^ A 'Quit' event is sent from somewhere inside KMonad
  deriving (Eq, Show, Generic)

instance Serialize Event

--------------------------------------------------------------------------------
-- $msg
--
-- 'Message's are the collection of messages that can be sent to the KMonad
-- daemon through its messaging socket.

-- | The different 'Message's that can be sent to KMonad
data Message
  = Shutdown           -- ^ Signal to quit
  | EmitKey !KeyAction -- ^ Signal to emit a key action to the OS
  deriving (Eq, Show, Generic)

instance Serialize Message

--------------------------------------------------------------------------------
-- $maps
--

-- | A mapping from 'Keycode's to 'Button's
data LayerCfg = LayerCfg
  { _lcName    :: !Name
  , _lcButtons :: !(M.HashMap Keycode ButtonCfg)
  }

-- |
type Layer = M.HashMap Keycode ButtonEnv


-- | A 'KeyMap' is a collection of 'Name'd maps from 'Keycode's to 'Button's
type KeyMap = MapStack Name Keycode ButtonEnv


--------------------------------------------------------------------------------
-- $server
--
-- Extra types to do with running the daemon server

type Port = ServiceName

class HasPort e where
  port :: Lens' e Port

--------------------------------------------------------------------------------
-- $cmd
--
-- The different things KMonad can be instructed to do.

-- | The different things KMonad can be instructed to do
data Command
  = StartDaemon !DaemonCfg
  | SendMessage !Port !Message
  | TestConfig !FilePath


--------------------------------------------------------------------------------
--



-- | 'RunCfg' is the minimum config with which KMonad is ever invoked.
data RunCfg = RunCfg
  { _LogLevel  :: !LogLevel -- ^ The minimum 'LogLevel' to display
  , _LogHandle :: !Handle   -- ^ Where to output logging
  , _Command   :: !Command  -- ^ The command used to invoke KMonad
  , _Verbose   :: !Bool     -- ^ Whether to be verbose
  }

-- | The 'MapCfg' describes how to load the keymap
data MapCfg
  = LoadKeyMapFromFile !FilePath -- ^ Read and parse a file
  | ParseFromText      !Text     -- ^ Parse 'Text' directly
  | TakeDirectly       !KeyMap   -- ^ Use `precompiled` KeyMap

-- | The 'DaemonCfg' describes the configuration of a the KMonad daemon
data DaemonCfg = DaemonCfg
  { _kcOpenKeySource :: !(Acquire KeySource)
  , _kcOpenKeySink   :: !(Acquire KeySink)
  , _kcPort          :: !Port
  , _kcMapCfg        :: !MapCfg
  , _kcRunCfg        :: !RunCfg
  }


makeClassy ''RunCfg
makeClassy ''MapCfg
makeLenses ''DaemonCfg

class (HasMapCfg e, HasRunCfg e) => HasDaemonCfg e where
  daemonCfg :: Lens' e DaemonCfg

  openKeySource :: Lens' e (Acquire KeySource)
  openKeySource = daemonCfg . kcOpenKeySource

  openKeySink :: Lens' e (Acquire KeySink)
  openKeySink = daemonCfg . kcOpenKeySink

instance HasMapCfg    DaemonCfg where mapCfg    = kcMapCfg
instance HasRunCfg    DaemonCfg where runCfg    = kcRunCfg
instance HasPort      DaemonCfg where port      = kcPort
instance HasDaemonCfg DaemonCfg where daemonCfg = id



