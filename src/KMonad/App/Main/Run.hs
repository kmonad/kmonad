module KMonad.App.Main.Run

where

import KMonad.Prelude

import KMonad.App.Invocation
import KMonad.App.KeyIO
import KMonad.App.Types
import KMonad.App.Parser.IO -- FIXME: change import when invoc/parse separation is clean
import KMonad.App.Main.Loop
import KMonad.App.Main.OS
import KMonad.Model hiding (withModel) -- FIXME: change when pullchain is factored out
import KMonad.Util hiding (logLvl)
import KMonad.Pullchain.IO

-- TODO: Fix bad naming of loglevel clashing between Cmd and Logging

{- NOTE:

The normal 'Types', 'IO', 'Operations', subdivision does not make a lot of sense
for 'Main', since *everything* is IO. However, since the module was getting
rather big, here are the main routines involved in setting up the environment
and starting the app-loop.

-}

--------------------------------------------------------------------------------
-- $init

-- | Initialize all the components of the KMonad app-loop
initAppEnv :: LUIO m e => AppCfg -> Ctx r m AppEnv
initAppEnv cfg = do

  -- Do any OS-related tweaks
  withOS

  -- Get a reference to the logging function
  lgf <- lift $ view logEnv

  -- Wait a bit for the user to release the 'Return' key with which they started
  -- KMonad. If we don't do this, we run the risk of capturing the keyboard used
  -- to start KMonad, resulting in a 'stuck' button.
  threadDelay $ (fromIntegral $ cfg^.startDelay) * 1000

  -- Acquire the keysource and keysink
  src <- withKeyInput  $ cfg^.keyInputCfg
  snk <- withKeyOutput $ cfg^.keyOutputCfg

  -- Initialize the model with the model config
  api <- withModel $ cfg^.modelCfg

  -- Gather it all up in out AppEnv
  pure $ AppEnv
    { _keAppCfg   = cfg
    , _keLogEnv   = lgf
    , _keySink    = snk
    , _keySource  = src
    , _aeModelAPI = api
    }

   
--------------------------------------------------------------------------------
-- $loop
--
-- All the top-level code used to start and continue KMonad's app-loop

-- | The entrypoint of KMonad
--
-- When called interactively: parse the 'Invoc' from the command-line and pass
-- it on to run.
--
-- NOTE: We separate 'main' from 'run', so 'run' could be used to execute
-- programatically defined 'Invoc's if so desired.
--
main :: OnlyIO ()
main = getInvoc >>= run


-- | Run KMonad using the provided configuration
startApp :: AppCfg -> OnlyLIO ()
startApp c = do
  runCtx (initAppEnv c) (flip runRIO loop)

-- | Execute the provided 'Cmd'
--
-- Wrapped in the context of OS-specific tweaks:
-- 1. Construct the log-func
-- 2. Parse the config-file
-- 3. Maybe start KMonad
--
-- TODO: This should dispatch more clearly on different tasks, tasks to add:
-- Key-Input mode: just print out key-events until interrupted.
-- Dry-Run mode: change dry-run mode from a flag to a command.
--
run :: Invoc -> OnlyIO ()
run c = do
  -- FIXME: Make this actually do something instead of constructing a default log-cfg
  let logcfg = LogCfg (c^.logLvl) stdout Nothing

  runLog logcfg $ do
    cfg <- loadConfig (c^.cfgFile) c -- Load cfg-file and overwrite Invoc settings
    unless (c^.dryRun) $ startApp cfg
