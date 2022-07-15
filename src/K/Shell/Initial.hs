-- |

module K.Shell.Initial
  ( -- * Basic types
    Task(..)
  , CmdAllow(..)
  , DelayRate(..)

    -- * OS differences
  , OS(..)
  , OSError(..)
  , AsOSError(..)

    -- * Util
  , UIO

  , module K.Initial
  , module K.Gesture
  , module K.Keyboard

  , module X
  )

where

import K.Initial
import K.Gesture
import K.Keyboard

import Control.Monad.Cont as X
import System.Info

-- basic types -----------------------------------------------------------------

-- | The task to execute during this kmonad run.
data Task
  = FullRun -- ^ Do a normal load-cfg-and-remap-my-keyboard run
  | CfgTest -- ^ Only try parsing all the configuration, then exit
  | EvTest  -- ^ Parse cfg, then run a read-print loop on the input keyboard
  | SendMsg -- ^ Send a message to KMonad
  deriving (Eq, Show)

-- | Different allowances for running shell-commands from kmonad
data CmdAllow
  = AllCmds  -- ^ Allow all commands
  | InitCmds -- ^ Allow only the commands involved in KeyIO initialization
  | NoCmds   -- ^ Allow no commands
  deriving (Eq, Show)

-- | Description of key-repeat behavior
data DelayRate = DelayRate
  { _delay :: Dt -- ^ How long to wait before repeating starts
  , _rate  :: Dt -- ^ How long between each repeat event
  } deriving (Eq, Show)

-- OS differences --------------------------------------------------------------

data OS
  = Linux
  | Windows
  | Mac
  | OtherOS Name
  deriving (Eq, Show)

currentOS :: OS
currentOS = case os of
  "darwin"  -> Mac
  "linux"   -> Linux
  "mingw32" -> Windows
  n         -> OtherOS $ pack n

data OSError = OSError OS Text
makeClassyPrisms ''OSError

instance Show OSError where
  show (OSError os t) = strUnlines
    [ "Trying to run code specific to " <> show os <> " on " <> show currentOS
    , "Error message: " <> unpack t]

instance Exception OSError
instance AsOSError SomeException where __OSError = _SomeException

-- util ------------------------------------------------------------------------

type UIO m = MonadUnliftIO m
