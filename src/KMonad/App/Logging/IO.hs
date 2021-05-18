module KMonad.App.Logging.IO

where

import KMonad.Prelude

import KMonad.Util.Ctx
import KMonad.App.Logging.Types

--------------------------------------------------------------------------------
-- $ctx

-- | Use 'LogCfg' to run a context-continuation
withLog :: UIO m => LogCfg -> Ctx r m LogEnv
withLog c = mkCtx $ \f -> do
  -- Default to non-verbose logging
  raw <- logOptionsHandle (c^.logTgt) False
  let ops = raw & setLogMinLevel (c^.logLvl)
  withLogFunc ops $ f . LogEnv c

-- | Run an Only-LogEnv RIO action in a UIO
runLog :: UIO m => LogCfg -> OnlyLIO a -> m a
runLog c = runCtx (withLog c) . inEnv


--------------------------------------------------------------------------------
-- $actions

-- | Print raw text to the screen, like putStrLn
say_ :: (LIO m env) => LogLevel -> Text -> m ()
say_ l a = view logEnv >>= \env -> runRIO env $ f (display a)
  where f = case l of
          LevelDebug -> logDebug
          LevelInfo  -> logInfo
          LevelWarn  -> logWarn
          LevelError -> logError
          _          -> logInfo

-- | Like 'say_' but already set to debug
say :: (LIO m env) => Text -> m ()
say = say_ LevelDebug

-- | Display an object by its 'Show' instance
print_ :: (LIO m env, Show a) => LogLevel -> a -> m ()
print_ l = say_ l . tshow

-- | Like 'print' but already set to LevelDebug
print :: (LIO m env, Show a) => a -> m ()
print = print_ LevelDebug

-- | Print section separators
section_ :: (LIO m env) => LogLevel -> m ()
section_ l = view (logEnv.logSep) >>= \case
  Nothing -> pure ()
  Just t  -> say_ l t

-- | Like 'section_' but already set to LevelDebug
section :: (LIO m env) => m ()
section = section_ LevelDebug

emptyLine :: (LIO m env) => m ()
emptyLine = say ""
