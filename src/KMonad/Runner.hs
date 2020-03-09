module KMonad.Runner
  ( kmonad
  , run
  )
where

import KPrelude

import KLisp
import KMonad.Daemon
import KMonad.Runner.Types
import KMonad.Runner.GetCmd

-- | Parse the arguments and then start run kmonad
kmonad :: IO ()
kmonad = getRunCfg >>= flip run dispatchCommand

-- | Dispatch on the command to run
dispatchCommand :: RIO RunEnv ()
dispatchCommand = view cmd >>= \case
  StartDaemon sd -> do
    dCfg <- loadConfig $ sd^.cfgFile
    runDaemon dCfg loop
  TestConfig (TestConfigCmd f) -> void $ loadConfig f





--------------------------------------------------------------------------------
-- $env
--
-- How to construct 'RunEnv's and run RIO RunEnv actions.

-- | Acquire a Handle to log to
withLogTarget :: LogTarget -> (Handle -> IO a) -> IO a
withLogTarget Stdout f       = f stdout
withLogTarget (ToFile pth) f = withFile pth WriteMode f

-- | Run a RIO RunEnv action by instantiating the 'RunEnv' from a 'RunCfg'
run :: RunCfg -> RIO RunEnv a -> IO a
run c rio =
  withLogTarget (c^.logTarget) $ \h -> do
    logOpts <- logOptionsHandle h False <&> setLogMinLevel (c^.logLevel)
    withLogFunc logOpts $ \lf -> do
      let renv = RunEnv
            { _reRunCfg  = c
            , _reLogFunc = lf
            }
      runRIO renv rio
