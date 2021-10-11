module KMonad.App.Parser.IO

where

import KMonad.Prelude

import KMonad.App.Invocation.Types
import KMonad.App.Invocation.Operations (joinCLI)

import KMonad.App.Types
import KMonad.App.Parser.Types
import KMonad.App.Parser.Tokenizer
import KMonad.App.Parser.TokenJoiner

import KMonad.Model.Types

data ParseResult
  = Success CfgToken
  | PError PErrors
  | JError JoinError
  deriving Show
makePrisms ''ParseResult

-- | Parse a config without throwing exceptions
parseConfig :: Text -> ParseResult
parseConfig f = case parseTokens f of
  Left per -> PError per
  Right es -> case joinKExprs es of
    Left jer -> JError jer
    Right ct -> Success ct

parseFile :: IO m => FilePath -> m ParseResult
parseFile = fmap parseConfig . readFileUtf8

-- | Parse a configuration file into a 'AppCfg' record
loadConfig :: HasLogFunc e => FilePath -> Invoc -> RIO e AppCfg
loadConfig pth cmd = do

  -- FIXME: We need to separate out the Cmd entirely from this.
  --
  -- What needs to happen:
  -- 1. We parse an invocation
  -- 2. If we need to load a config, we load it
  -- 3. We overwrite the loaded config with options from the invocation
  -- 4. We use the result to start KMonad
  --
  -- What is happening:
  -- Everything at the same time

  tks <- loadTokens pth                 -- This can throw a PErrors
  cgt <- joinConfigIO (joinCLI cmd tks) -- This can throw a JoinError

  -- Assemble the AppCfg record
  pure $ AppCfg
    { _keyInputCfg  = _src   cgt
    , _keyOutputCfg = _snk   cgt
    , _acModelCfg   = ModelCfg
        { _keymapCfg  = _km    cgt
        , _firstLayer = _fstL  cgt
        , _fallThrough  = _flt cgt
        , _mAllowCmd     = _allow cgt
        }
    , _allowCmd     = _allow cgt
    , _startDelay   = _strtDel cmd
    }
