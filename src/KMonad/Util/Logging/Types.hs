module KMonad.Util.Logging.Types
  ( LogCfg(..)
  , LogEnv(..)
  , LogLevel(..)

  , HasLogCfg(..)
  , HasLogEnv(..)

  , LIO, LUIO, OnlyLIO
  )
where


import KMonad.Prelude
import RIO (LogLevel(..))
import qualified RIO.Text as T


--------------------------------------------------------------------------------
-- $cfg

-- | The full logging configuration for KMonad
data LogCfg = LogCfg
  { _logLvl :: !LogLevel     -- ^ What level to log at
  , _logTgt :: !Handle       -- ^ Where to log to
  , _logSep :: !(Maybe Text) -- ^ If and what to use as section separator
  } deriving Show
makeClassy ''LogCfg

instance Default LogCfg where
  def = LogCfg
    { _logLvl = LevelInfo
    , _logTgt = stdout
    , _logSep = Just $ "\n" <> T.replicate 80 "-"
    }


--------------------------------------------------------------------------------
-- $env

-- | The logging runtime environment
data LogEnv = LogEnv
  { _leLogCfg  :: !LogCfg -- ^ Copy of the config
  , _logFunc :: !LogFunc  -- ^ RIO LogFunc
  }
makeClassy ''LogEnv

instance HasLogCfg  LogEnv where logCfg   = leLogCfg
instance HasLogFunc LogEnv where logFuncL = logFunc


--------------------------------------------------------------------------------
-- $shorthand

-- | Type shorthand for ReaderT-IO-with-logging
type LIO m env  = (EnvIO m env, HasLogEnv env, HasLogCfg env)
type LUIO m env = (LIO m env, UIO m)
type OnlyLIO a  = RIO LogEnv a
