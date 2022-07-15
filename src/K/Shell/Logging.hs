-- |

module K.Shell.Logging where

import K.Shell.Initial
import K.Shell.Cfg

import Text.Pretty.Simple
import RIO.Text.Lazy (toStrict)

import qualified RIO      as R
import qualified RIO.Text as T

-- basic types -----------------------------------------------------------------

-- | The runtime environment for logging
data LogEnv = LogEnv
  { _leLogCfg :: !LogCfg   -- ^ Copy of the config
  , _logAt    :: !LogLevel -- ^ Current level of logging
  , _logFunc  :: !LogFunc  -- ^ RIO LogFunc
  }
makeClassy ''LogEnv

instance HasLogCfg  LogEnv where logCfg   = leLogCfg
instance HasLogFunc LogEnv where logFuncL = logFunc

type CanLog env m = (MonadReader env m, HasLogEnv env, MonadIO m)

-- initialization --------------------------------------------------------------

-- | Use 'LogCfg' to run an action in the context of an initialized 'LogEnv'
withLogging :: (UIO m, HasLogCfg c) => c -> Ctx r m LogEnv
withLogging c = ContT $ \f -> do
  raw <- logOptionsHandle (c^.logTarget) False
  withLogFunc (raw & setLogMinLevel (c^.logLevel)) $
    f . LogEnv (c^.logCfg) LevelDebug

-- ops -------------------------------------------------------------------------

-- | Express some text at the current level
log :: CanLog env m => Text -> m ()
log t = view logAt >>= \case
  LevelDebug -> liftLog R.logDebug t
  LevelInfo  -> liftLog R.logInfo  t
  LevelWarn  -> liftLog R.logWarn  t
  LevelError -> liftLog R.logError t
  _          -> devFail "Custom log-levels don't occur in KMonad"
  where liftLog f t = view logFunc >>= \lf -> runRIO lf . f . display $ t

-- | Pretty-print a haskell-value
pp :: (CanLog env m, Show a) => a -> m ()
pp a = do
  f <- view (logEnv.logColor) >>= \case
    DarkBG     -> pure pShow
    LightBG    -> pure pShowLightBg
    Monochrome -> pure pShowNoColor
  log . toStrict . f $ a

-- | Print a value by its display instance
dsp :: (CanLog env m, Display a) => a -> m ()
dsp = log . textDisplay

-- | Print out the logging separator
sep :: CanLog env m => m ()
sep = view (logEnv.useSep) >>= \b -> when b (log $ "\n" <> T.replicate 80 "-")

-- | Print a separator and then log some text
sepLog :: CanLog env m => Text -> m ()
sepLog = const sep <=< log

-- | Set the current log level to some value for a statement
--
-- Note the difference between RIO logging and KMonad logging. In RIO logging
-- looks like this:
-- logDebug $ some statement
-- logDebug $ some other statement
--
-- In KMonad, logging looks like this:
-- atDebug $ do
--   log $ some statement
--   log $ some other statement
atDebug, atInfo, atWarn, atError :: CanLog env m => m a -> m a
atDebug = locally logAt (const LevelDebug)
atInfo  = locally logAt (const LevelInfo)
atWarn  = locally logAt (const LevelWarn)
atError = locally logAt (const LevelError)

-- | Shorthand for @atLevel . log@
logDebug, logInfo, logWarn, logError :: CanLog env m => Text -> m ()
logDebug = atDebug . log
logInfo  = atInfo . log
logWarn  = atWarn . log
logError = atError . log
