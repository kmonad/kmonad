{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module KMonad.Runner
  ( RunCfg
  , defRunCfg
  , verbose
  , logHandle
  , logLevel

  , RunEnv
  , HasRunEnv(..)
  , run
  )
where

import Prelude

import KMonad.Util

--------------------------------------------------------------------------------
-- $cfg

-- | The RunCfg describes those settings that apply to every part of KMonad.
-- This currently only really has to do with logging configuration.
data RunCfg = RunCfg
  { _verbose   :: !Bool
  , _logHandle :: !Handle
  , _logLevel  :: !LogLevel
  }
makeClassy ''RunCfg

-- | The default settings for 'RunCfg'
defRunCfg :: RunCfg
defRunCfg = RunCfg
  { _verbose   = False
  , _logHandle = stdout
  , _logLevel  = LevelWarn
  }

-- | The RunEnv contains the environment that is present in all computations in
-- KMonad. It currently only really describes logging.
data RunEnv = RunEnv
  { __cfg      :: RunCfg
  , _reLogFunc :: LogFunc
  }
makeLenses ''RunEnv

instance {-# OVERLAPS #-} (HasRunEnv e) => HasRunCfg e where
  runCfg = runEnv . runCfg
instance {-# OVERLAPS #-} (HasRunEnv e) => HasLogFunc e where
  logFuncL = runEnv . reLogFunc


class (HasLogFunc e, HasRunCfg e) => HasRunEnv e where
  runEnv :: Lens' e RunEnv
instance HasRunEnv RunEnv where runEnv = id

--------------------------------------------------------------------------------

-- | Run a RIO RunEnv action by instantiation the 'RunEnv' from a 'RunCfg'
run :: RunCfg -> RIO RunEnv a -> IO a
run c rio = do
  lo <- logOptionsHandle stdout (c^.verbose) <&> setLogMinLevel (c^.logLevel)
  withLogFunc lo $ \lf -> do
    let renv = RunEnv
          { __cfg      = c
          , _reLogFunc = lf
          }
    runRIO renv rio
