{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module KMonad.Runner.Types


where

import KPrelude


data StartDaemonCmd = StartDaemonCmd
  { _cfgFile  :: FilePath
  }
  deriving Show
makeLenses ''StartDaemonCmd

data TestConfigCmd = TestConfigCmd FilePath
  deriving Show

data LogTarget
  = Stdout
  | ToFile FilePath
  deriving Show

data Command
  = StartDaemon StartDaemonCmd
  | TestConfig  TestConfigCmd
  -- | SendMessage Port Message
  deriving Show


data RunCfg = RunCfg
  { _cmd       :: !Command
  , _logLevel  :: !LogLevel
  , _logTarget :: !LogTarget
  } deriving Show
makeClassy ''RunCfg


--------------------------------------------------------------------------------
-- $cfg

-- | The RunCfg describes those settings that apply to every part of KMonad.
-- This currently only really has to do with logging configuration.


-- | The RunEnv contains the environment that is present in all computations in
-- KMonad. It currently only really describes logging.
data RunEnv = RunEnv
  { _reRunCfg    :: RunCfg
  , _reLogFunc :: LogFunc
  }
makeLenses ''RunEnv

instance {-# OVERLAPS #-} (HasRunEnv e) => HasRunCfg e where
  runCfg   = runEnv . reRunCfg
instance {-# OVERLAPS #-} (HasRunEnv e) => HasLogFunc e where
  logFuncL = runEnv . reLogFunc

class (HasLogFunc e, HasRunCfg e) => HasRunEnv e where
  runEnv :: Lens' e RunEnv
instance HasRunEnv RunEnv where runEnv = id
