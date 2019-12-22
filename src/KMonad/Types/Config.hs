module KMonad.Types.Config

where

import KMonad.Prelude
import KMonad.Types.Message

-- The types of configs used in multiple locations in KMonad

data NetworkCfg = NetworkCfg
  { _port :: Port
  }

data LoggingCfg = LogginCfg
  { _LogLevel  :: !LogLevel -- ^ The minimum 'LogLevel' to display
  , _LogHandle :: !Handle   -- ^ Where to output logging
  , _Verbose   :: !Bool     -- ^ Whether to be verbose
  }
