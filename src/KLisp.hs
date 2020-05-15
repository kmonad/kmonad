{-|
Module      : KLisp
Description : The module responsible for parsing the configuration file
Copyright   : (c) David Janssen, 2019
License     : MIT

Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : non-portable (MPTC with FD, FFI to Linux-only c-code)

-}
module KLisp
  ( loadConfig
  , PErrors(..)
  )
where

import KPrelude

import KMonad.App

import KLisp.Parser
import KLisp.Joiner
import KLisp.Types

-- | Parse a configuration file into a 'DaemonCfg' record
loadConfig :: HasLogFunc e => FilePath -> RIO e AppCfg
loadConfig pth = do
  lf  <- view logFuncL
  tks <- loadTokens pth   -- This can throw a parse-error
  cfg <- joinConfigIO tks -- This can throw a JoinError

  -- Try loading the sink and src
  snk <- liftIO . _snk cfg $ lf
  src <- liftIO . _src cfg $ lf

  -- Assemble the DaemonCfg record
  pure $ DaemonCfg
    { _keySinkDev   = snk
    , _keySourceDev = src
    , _keymapCfg    = _km   cfg
    , _firstLayer   = _fstL cfg
    , _port         = _prt  cfg
    }
