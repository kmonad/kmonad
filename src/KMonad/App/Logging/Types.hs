module KMonad.App.Logging.Types

where


import KMonad.Prelude

import qualified RIO as R (LogLevel(..))
import qualified RIO.Text as T


{-
RIO-logging is a bit too configurable, so we specify a simpler subset of
possible logging behaviors.
-}


--------------------------------------------------------------------------------
-- $levels

-- | The log-levels we support
data LogLevel
  = LevelError  -- ^ Write only errors
  | LevelInfo   -- ^ Also write status updates
  | LevelDebug  -- ^ Also write full reports
  deriving Show

asRIO :: Getter LogLevel R.LogLevel
asRIO = to $ \case
  LevelError -> R.LevelError
  LevelInfo  -> R.LevelInfo
  LevelDebug -> R.LevelDebug

--------------------------------------------------------------------------------
-- $cfg

-- | The full logging configuration provided by "KMonad.Cmd.getCmd"
data LogCfg = LogCfg
  { _logLvl :: !LogLevel     -- ^ What level to log at
  , _logTgt :: !Handle       -- ^ Where to log to
  , _logSep :: !(Maybe Text) -- ^ If and what to use as section separator
  } deriving Show
makeClassy ''LogCfg

instance Default LogCfg where
  def = LogCfg
    { _logLvl = LevelDebug
    , _logTgt = stdout
    , _logSep = Just $ "\n" <> T.replicate 80 "-" }

--------------------------------------------------------------------------------
-- $env

-- | The logging runtime env
data LogEnv = LogEnv
  { _leLogCfg  :: !LogCfg  -- ^ Copy of the config
  , _leLogFunc :: !LogFunc -- ^ RIO LogFunc
  }
makeClassy ''LogEnv

instance HasLogCfg  LogEnv where logCfg   = leLogCfg
instance HasLogFunc LogEnv where logFuncL = leLogFunc


--------------------------------------------------------------------------------
-- $shorthand

-- | Type shorthand for ReaderT-IO-with-logging
type LIO m env  = (EnvIO m env, HasLogEnv env)
type LUIO m env = (LIO m env, UIO m)
type OnlyLIO a = RIO LogEnv a
