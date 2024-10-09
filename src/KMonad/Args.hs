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
  ( getCmdL, loadConfig, CmdL, HasCmdL(..))
where

import KMonad.App.Types
import KMonad.Args.Cmd
import KMonad.Args.Joiner
import KMonad.Args.Parser
import KMonad.Model.Cfg

--------------------------------------------------------------------------------
--

-- | Parse a configuration file into a 'ACfg' record
loadConfig :: HasLogFunc e => CmdL -> RIO e ACfg
loadConfig cmdl = do

  tks <- loadTokens (cmdl^.keymap)               -- This can throw a ParseError
  tcfg <- joinConfigIO (joinCLI (cmdl^.cfg) tks) -- This can throw a JoinError

  -- Try loading the sink and src
  lf  <- view logFuncL
  snk <- liftIO $ tcfg^.sink   $ lf
  src <- liftIO $ tcfg^.source $ lf

  -- Assemble the AppCfg record
  pure $ ACfg
    { _sink   = snk
    , _source = src
    , _keymap    = tcfg^.keymap
    , _fstLayer   = tcfg^.fstLayer
    , _fallThrough  = tcfg^.fallThrough
    , _allowCmd     = tcfg^.allowCmd
    , _startDelay   = cmdl^.startDelay
    , _keySeqDelay  = fromIntegral <$> tcfg^.keySeqDelay
    }


-- | Join the options given from the command line with the one read from the
-- configuration file.
-- This does not yet throw any kind of exception, as we are simply overwriting an option
-- but don't fix duplication issues.
-- They will be handled while joining.
joinCLI :: CCfg -> PCfg -> PCfg
joinCLI (CCfg i o _ ck csd _ ft ac _ ksd ia) pcfg =
  pcfg
    & source      %~ overwriteOption i
    & sink        %~ overwriteOption o
    & cmpKey      %~ overwriteOption ck
    & cmpSeqDelay %~ overwriteOption csd
    & fallThrough %~ overwriteOption ft
    & allowCmd    %~ overwriteOption ac
    & keySeqDelay %~ overwriteOption ksd
    & implArnd    %~ overwriteOption ia
 where
  -- | Overwrite an option but don't fix duplication issues, since they will be handled later.
  -- Though we do allow adding a missing option, which is weird when it is a required option.
  overwriteOption :: Maybe a -> [a] -> [a]
  overwriteOption Nothing vs = vs
  overwriteOption (Just _) vs@(_ : _ : _) = vs
  overwriteOption (Just v) _ = [v]
