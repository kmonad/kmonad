module KMonad.App.Parser.IO

where

import KMonad.Prelude

import KMonad.App.Types
import KMonad.App.Parser.Types
import KMonad.App.Parser.Tokenizer (loadTokens)
import KMonad.App.Parser.TokenJoiner (joinConfigIO)

-- | Parse a configuration file into a 'AppCfg' record
loadConfig :: HasLogFunc e => FilePath -> RIO e CfgToken
loadConfig pth = do

  -- FIXME: Continue here:
  --
  -- Next steps: You need to separate the concerns of config parsing and
  -- invocation. The parsing need only deliver a record of tokens and should be
  -- independent of the invocation. The invocation can depend on the types from
  -- the parser, and should allow for changing a config-record.
  --
  -- That config-record should then be handed of to the app-loop, which uses it
  -- to initialize KeyIO and such.
  --
  -- After that is sane, it makes sense to start tinkering with KeyIO
 
  tks <- loadTokens pth                 -- This can throw a PErrors
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
