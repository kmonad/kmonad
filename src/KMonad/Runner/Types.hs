module KMonad.Runner.Types

where

import KMonad.Prelude

import KMonad.Daemon.Types
import KMonad.Types.Message

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
-- $runcfg

-- | 'RunCfg' is the minimum config with which KMonad is ever invoked.
data RunCfg = RunCfg
  { _LogLevel  :: !LogLevel -- ^ The minimum 'LogLevel' to display
  , _LogHandle :: !Handle   -- ^ Where to output logging
  , _Command   :: !Command  -- ^ The command used to invoke KMonad
  , _Verbose   :: !Bool     -- ^ Whether to be verbose
  }
makeClassy ''RunCfg
