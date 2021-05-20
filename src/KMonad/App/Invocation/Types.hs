module KMonad.App.Invocation.Types
  ( Invoc(..)
  , HasInvoc(..)
  )
where

import KMonad.Prelude hiding (try)
import KMonad.Util.Time
import KMonad.Util.Logging hiding (logLvl)

import KMonad.App.Parser --(itokens, keywordButtons, noKeywordButtons, otokens, symbol)
-- import KMonad.Parser.Types (DefSetting(..), choice, try)

-- import qualified KMonad.Args.Types as M  -- [M]egaparsec functionality

import Options.Applicative


--------------------------------------------------------------------------------
-- $cmd
--
-- The different things KMonad can be instructed to do.

-- | Record describing the instruction to KMonad
data Invoc = Invoc
  { _cfgFile   :: FilePath -- ^ Which file to read the config from
  , _dryRun    :: Bool     -- ^ Flag to indicate we are only test-parsing
  , _logLvl    :: LogLevel -- ^ Level of logging to use
  , _strtDel   :: Ms       -- ^ How long to wait before acquiring the input keyboard

    -- All 'KDefCfg' options of a 'KExpr'
  , _cmdAllow  :: DefSetting       -- ^ Allow execution of arbitrary shell-commands?
  , _fallThrgh :: DefSetting       -- ^ Re-emit unhandled events?
  , _initStr   :: Maybe DefSetting -- ^ TODO: What does this do?
  , _cmpSeq    :: Maybe DefSetting -- ^ Key to use for compose-key sequences
  , _oToken    :: Maybe DefSetting -- ^ How to emit the output
  , _iToken    :: Maybe DefSetting -- ^ How to capture the input
  }
  deriving Show
makeClassy ''Invoc
