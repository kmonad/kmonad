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
  { _command   :: !Command          -- ^ The command used to invoke KMonad
  , _logLevel  :: !LogLevel         -- ^ The minimum 'LogLevel' to display
  , _logHandle :: !Handle           -- ^ Where to output logging
  , _verbose   :: !Bool             -- ^ Whether to be verbose
  , _cfgFile   :: !(Maybe FilePath) -- ^ Where to load configurations
  , _port      :: !(Maybe Port)     -- ^ The network port to listen on
  }
makeClassy ''RunCfg


getPort :: HasRunCfg e => RIO e Port
getPort = fromMaybe loadPort =<< view port
