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
  ( run )
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

-- | Execute the provided 'Cmd'
--
-- 1. Construct the log-func
-- 2. Parse the config-file
-- 3. Maybe start KMonad
runCmd :: Cmd -> IO ()
runCmd c = do
  o <- logOptionsHandle stdout False <&> setLogMinLevel (c^.logLvl)
  withLogFunc o $ \f -> runRIO f $ do
    cfg <- loadConfig c
    unless (c^.dryRun) $ startApp cfg

-- | Parse a configuration file into a 'AppCfg' record
loadConfig :: HasLogFunc e => Cmd -> RIO e AppCfg
loadConfig cmd = do

  tks <- loadTokens (cmd^.cfgFile)      -- This can throw a PErrors
  cgt <- joinConfigIO (joinCLI cmd tks) -- This can throw a JoinError

  -- Try loading the sink and src
  lf  <- view logFuncL
  snk <- liftIO . _snk cgt $ lf
  src <- liftIO . _src cgt $ lf

  -- Assemble the AppCfg record
  pure $ AppCfg
    { _keySinkDev   = snk
    , _keySourceDev = src
    , _keymapCfg    = _km    cgt
    , _firstLayer   = _fstL  cgt
    , _fallThrough  = _flt   cgt
    , _allowCmd     = _allow cgt
    }

-- | Join the options given from the command line with the one read from the
-- configuration file.
-- This does not yet throw any kind of exception, as we are simply inserting the
-- given options into every 'KDefCfg' block that we see.
joinCLI :: Cmd -> [KExpr] -> [KExpr]
joinCLI cmd = traverse._KDefCfg %~ insertCliOption cliList
 where
  -- | All options and flags that were given on the command line.
  cliList :: DefSettings
  cliList = catMaybes $
       map flagToMaybe [cmd^.cmdAllow, cmd^.fallThrgh]
    <> [cmd^.iToken, cmd^.oToken, cmd^.cmpSeq, cmd^.initStr]

  -- | Convert command line flags to a 'Maybe' type, where the non-presence, as
  -- well as the default value of a flag will be interpreted as @Nothing@
  flagToMaybe :: DefSetting -> Maybe DefSetting
  flagToMaybe = \case
    SAllowCmd    b -> if b then Just (SAllowCmd    b) else Nothing
    SFallThrough b -> if b then Just (SFallThrough b) else Nothing
    _              -> Nothing

  -- | Insert all command line options, potentially overwriting already existing
  -- options that were given in the configuration file. This is a paramorphism
  insertCliOption :: DefSettings -> DefSettings -> DefSettings
  insertCliOption cliSettings cfgSettings =
    foldr (\s cfgs ->
             if   s `elem` cfgs
             then foldr (\x xs -> (if s == x then s else x) : xs) [] cfgs
             else s : cfgs)
          cfgSettings
          cliSettings
