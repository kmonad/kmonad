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
  (loadConfig)
where

import KPrelude
import KMonad.App


-- | Parse a configuration file into a 'DaemonCfg' record
loadConfig :: HasLogFunc e => FilePath -> RIO e AppCfg
loadConfig pth = do
  undefined
  -- lf  <- view logFuncL
  -- tks <- loadTokens pth   -- This can throw a parse-error
  -- cfg <- joinConfigIO tks -- This can throw a JoinError

  -- -- Try loading the sink and src
  -- snk <- liftIO . _snk cfg $ lf
  -- src <- liftIO . _src cfg $ lf

  -- -- Assemble the DaemonCfg record
  -- pure $ AppCfg
  --   { _keySinkDev   = snk
  --   , _keySourceDev = src
  --   , _keymapCfg    = _km   cfg
  --   , _firstLayer   = _fstL cfg
  --   }
