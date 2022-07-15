-- |

module K.Shell.Env.Initial
  ( IvkCtxEnv(..)
  , CfgCtxEnv(..)
  , AcqCtxEnv(..)

  , HasIvkCtxEnv(..)
  , HasCfgCtxEnv(..)

  , IvkM
  , CfgM
  , AcqM

  , module K.Shell.Initial
  , module K.Shell.Cfg
  , module K.Shell.Logging
  , module K.Shell.KeyIO
  )
where

import K.Shell.Initial
import K.Shell.Cfg
import K.Shell.Logging
import K.Shell.KeyIO

import Control.Monad.Catch

-- runtime environment ---------------------------------------------------------

data IvkCtxEnv = IvkCtxEnv
  { _iShellCfg :: ShellCfg
  , _iLogEnv   :: LogEnv
  , _invoc    :: Invoc
  }
makeClassy ''IvkCtxEnv

instance HasLogEnv IvkCtxEnv where logEnv = iLogEnv
instance HasShellCfg IvkCtxEnv where shellCfg = iShellCfg
instance HasRunCfg IvkCtxEnv where runCfg = shellCfg.runCfg

data CfgCtxEnv = CfgCtxEnv
  { _cShellCfg :: ShellCfg
  , _cLogEnv   :: LogEnv
  }
makeClassy ''CfgCtxEnv

instance HasLogEnv CfgCtxEnv where logEnv = cLogEnv
instance HasShellCfg CfgCtxEnv where shellCfg = cShellCfg
instance HasLocale CfgCtxEnv where locale = shellCfg.locale
instance HasRunCfg CfgCtxEnv where runCfg = shellCfg.runCfg

data AcqCtxEnv = AcqCtxEnv
  { _aShellCfg :: ShellCfg
  , _aLogEnv   :: LogEnv
  , _aKioEnv   :: KioEnv
  }
makeClassy ''AcqCtxEnv

instance HasLogEnv AcqCtxEnv where logEnv = aLogEnv
instance HasKioEnv AcqCtxEnv where kioEnv = aKioEnv
instance HasShellCfg AcqCtxEnv where shellCfg = aShellCfg

-- shorthand -------------------------------------------------------------------

-- | The different capacity levels the shell monad can run at

-- | Shorthand for a
type IvkM a = RIO IvkCtxEnv a

-- | Medium capacity level
-- 4. access to cfg default overwritten by cfgfile overwritten by invoc
-- 5. since locale is cfg-file-only, locale is now available
type CfgM = RIO CfgCtxEnv

-- | Full capacity
-- 6. access to an acquired keysource, sink, and repeat environment
type AcqM a = RIO AcqCtxEnv a
