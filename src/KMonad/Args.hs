{-|
Module      : KMonad.Args
Description : How to parse arguments and config files into an AppCfg
Copyright   : (c) David Janssen, 2019
License     : MIT

Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : non-portable (MPTC with FD, FFI to Linux-only c-code)

-}
module KMonad.Args
  ( run
  , test -- TODO: remove me when done
  )
where

import KMonad.Prelude
import KMonad.App
import KMonad.Args.Cmd
import KMonad.Args.Joiner
import KMonad.Args.Parser
import KMonad.Args.Types

--------------------------------------------------------------------------------
--

-- | Run KMonad
run :: IO ()
run = getCmd >>= runCmd

-- | Start KMonad with the 'example.kbd' config file
test :: IO ()
test = runCmd $ Cmd "../../../doc/example.kbd" False LevelDebug

-- | Execute the provided 'Cmd'
--
-- 1. Construct the log-func
-- 2. Parse the config-file
-- 3. Maybe start KMonad
runCmd :: Cmd -> IO ()
runCmd c = do
  o <- logOptionsHandle stdout False <&> setLogMinLevel (c^.logLvl)
  withLogFunc o $ \f -> runRIO f $ do
    cfg <- loadConfig $ c^.cfgFile
    unless (c^.dryRun) $ startApp cfg


-- | Parse a configuration file into a 'AppCfg' record
loadConfig :: HasLogFunc e => FilePath -> RIO e AppCfg
loadConfig pth = do

  tks <- loadTokens pth   -- This can throw a PErrors
  cgt <- joinConfigIO tks -- This can throw a JoinError

  -- Try loading the sink and src
  lf  <- view logFuncL
  snk <- liftIO . _snk cgt $ lf
  src <- liftIO . _src cgt $ lf

  -- Assemble the AppCfg record
  pure $ AppCfg
    { _keySinkDev   = snk
    , _keySourceDev = src
    , _keymapCfg    = _km   cgt
    , _firstLayer   = _fstL cgt
    }
