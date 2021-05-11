{-|
Module      : KMonad.Args.Cmd
Description : Parse command-line options into a 'Cmd' for KMonad to execute
Copyright   : (c) David Janssen, 2019
License     : MIT

Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : non-portable (MPTC with FD, FFI to Linux-only c-code)

-}
module KMonad.Args.Cmd
  ( Cmd(..)
  , HasCmd(..)
  , getCmd
  )
where

import KMonad.Prelude hiding (try)
import KMonad.Args.Parser (itokens, keywordButtons, noKeywordButtons, otokens, symbol)
import KMonad.Args.Types (DefSetting(..), choice, try)

import qualified KMonad.Args.Types as M  -- [M]egaparsec functionality

import Options.Applicative


--------------------------------------------------------------------------------
-- $cmd
--
-- The different things KMonad can be instructed to do.

-- | Record describing the instruction to KMonad
data Cmd = Cmd
  { _cfgFile   :: FilePath -- ^ Which file to read the config from
  , _dryRun    :: Bool     -- ^ Flag to indicate we are only test-parsing
  , _logLvl    :: LogLevel -- ^ Level of logging to use

    -- All 'KDefCfg' options of a 'KExpr'
  , _cmdAllow  :: DefSetting       -- ^ Allow execution of arbitrary shell-commands?
  , _fallThrgh :: DefSetting       -- ^ Re-emit unhandled events?
  , _initStr   :: Maybe DefSetting -- ^ TODO: What does this do?
  , _cmpSeq    :: Maybe DefSetting -- ^ Key to use for compose-key sequences
  , _oToken    :: Maybe DefSetting -- ^ How to emit the output
  , _iToken    :: Maybe DefSetting -- ^ How to capture the input
  }
  deriving Show
makeClassy ''Cmd

-- | Parse 'Cmd' from the evocation of this program
getCmd :: IO Cmd
getCmd = customExecParser (prefs showHelpOnEmpty) $ info (cmdP <**> helper)
  (  fullDesc
  <> progDesc "Start KMonad"
  <> header   "kmonad - an onion of buttons."
  )


--------------------------------------------------------------------------------
-- $prs
--
-- The different command-line parsers

-- | Parse the full command
cmdP :: Parser Cmd
cmdP =
  Cmd <$> fileP
      <*> dryrunP
      <*> levelP
      <*> cmdAllowP
      <*> fallThrghP
      <*> initStrP
      <*> cmpSeqP
      <*> oTokenP
      <*> iTokenP

-- | Parse a filename that points us at the config-file
fileP :: Parser FilePath
fileP = strArgument
  (  metavar "FILE"
  <> help    "The configuration file")

-- | Parse a flag that allows us to switch to parse-only mode
dryrunP :: Parser Bool
dryrunP = switch
  (  long    "dry-run"
  <> short   'd'
  <> help    "If used, do not start KMonad, only try parsing the config file"
  )

-- | Parse the log-level as either a level option or a verbose flag
levelP :: Parser LogLevel
levelP = option f
  (  long    "log-level"
  <> short   'l'
  <> metavar "Log level"
  <> value   LevelWarn
  <> help    "How much info to print out (debug, info, warn, error)" )
  where
    f = maybeReader $ flip lookup [ ("debug", LevelDebug), ("warn", LevelWarn)
                                  , ("info",  LevelInfo),  ("error", LevelError) ]

-- | Allow the execution of arbitrary shell-commands
cmdAllowP :: Parser DefSetting
cmdAllowP = SAllowCmd <$> switch
  (  long "allow-cmd"
  <> short 'c'
  <> help "Whether to allow the execution of arbitrary shell-commands"
  )

-- | Re-emit unhandled events
fallThrghP :: Parser DefSetting
fallThrghP = SFallThrough <$> switch
  (  long "fallthrough"
  <> short 'f'
  <> help "Whether to simply re-emit unhandled events"
  )

-- | TODO what does this do?
initStrP :: Parser (Maybe DefSetting)
initStrP = optional $ SInitStr <$> strOption
  (  long "init"
  <> short 't'
  <> metavar "STRING"
  <> help "TODO"
  )

-- | Key to use for compose-key sequences
cmpSeqP :: Parser (Maybe DefSetting)
cmpSeqP = optional $ SCmpSeq <$> option
  (tokenParser keywordButtons <|> megaReadM (choice noKeywordButtons))
  (  long "cmp-seq"
  <> short 's'
  <> metavar "BUTTON"
  <> help "Which key to use to emit compose-key sequences"
  )

-- | Where to emit the output
oTokenP :: Parser (Maybe DefSetting)
oTokenP = optional $ SOToken <$> option (tokenParser otokens)
  (  long "output"
  <> short 'o'
  <> metavar "OTOKEN"
  <> help "Emit output to OTOKEN"
  )

-- | How to capture the keyboard input
iTokenP :: Parser (Maybe DefSetting)
iTokenP = optional $ SIToken <$> option (tokenParser itokens)
  (  long "input"
  <> short 'i'
  <> metavar "ITOKEN"
  <> help "Capture input via ITOKEN"
  )

-- | Transform a bunch of tokens of the form @(Keyword, Parser)@ into an
-- optparse-applicative parser
tokenParser :: [(Text, M.Parser a)] -> ReadM a
tokenParser = megaReadM . choice . map (try . uncurry ((*>) . symbol))

-- | Megaparsec <--> optparse-applicative interface
megaReadM :: M.Parser a -> ReadM a
megaReadM p = eitherReader (mapLeft show . M.parse p "" . fromString)
