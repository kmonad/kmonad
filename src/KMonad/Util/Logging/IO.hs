module KMonad.Util.Logging.IO
  ( -- * $run
    withLog
  , runLog

    -- * $log
  , logError, logWarn, logInfo, logDebug
  , dspError, dspWarn, dspInfo, dspDebug
  , sepError, sepWarn, sepInfo, sepDebug
  )
where

import KMonad.Prelude

import KMonad.Util.Ctx
import KMonad.Util.Logging.Types

import qualified RIO as R

--------------------------------------------------------------------------------
-- $run

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
-- $log

-- | Turn a RIO logXXX function into one that uses 'LogEnv' and runs on Text.
--
-- I've always found RIO logging to be ever slightly too cumbersome to use
-- happily. This slightly simplified version feels more pleasant to work with in
-- an environment as simple as KMonad.
--
-- NOTE: This function is just used to generate the logXXXX functions for
-- KMonad, we don't export it.
simplify :: (LIO m e) => (Utf8Builder -> RIO R.LogFunc ()) -> (Text -> m ())
simplify f t = view logFunc >>= \lf -> runRIO lf . f . display $ t

-- | Functions to write text to the log.
logError, logWarn, logInfo, logDebug :: (LIO m e) => Text -> m ()
logError = simplify R.logError
logWarn  = simplify R.logWarn
logInfo  = simplify R.logInfo
logDebug = simplify R.logDebug

-- | Functions to write displayable items to the log.
dspError, dspWarn, dspInfo, dspDebug :: (LIO m e, Display a) => a -> m ()
dspError = logError . textDisplay
dspWarn  = logWarn  . textDisplay
dspInfo  = logInfo  . textDisplay
dspDebug = logDebug . textDisplay

-- | Functions to write separators to the log.
sepError, sepWarn, sepInfo, sepDebug :: (LIO m e) => m ()
sepError = traverse_ logError =<< view logSep
sepWarn  = traverse_ logWarn  =<< view logSep
sepInfo  = traverse_ logInfo  =<< view logSep
sepDebug = traverse_ logDebug =<< view logSep

