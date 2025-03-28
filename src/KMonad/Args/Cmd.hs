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
  ( CmdL(..)
  , HasCmdL(..)
  , getCmdL
  )
where

import qualified KMonad.Args.Parser as P
import KMonad.Args.TH (gitHash)
import KMonad.Model
import Paths_kmonad (version)

import qualified KMonad.Parsing as M  -- [M]egaparsec functionality

import Data.Version (showVersion)
import Options.Applicative


--------------------------------------------------------------------------------
-- $cmd
--
-- The different things KMonad can be instructed to do.

-- | Record describing the instruction to KMonad on the command line.
data CmdL = CmdL
  { _ccfg :: CCfg         -- ^ Non command line specific options
  , _dryRun    :: Bool         -- ^ Flag to indicate we are only test-parsing
  }
makeClassy ''CmdL
instance HasCfg CmdL 'Cmd where
  cfg f x@CmdL{_ccfg = c} = f c <&> \c' -> x{_ccfg = c'}

-- | Parse 'Cmd' from the evocation of this program
getCmdL :: IO CmdL
getCmdL = customExecParser (prefs showHelpOnEmpty) $
  info (cmdLP <**> versioner <**> helper)
    (  fullDesc
    <> progDesc "Start KMonad"
    <> header   "kmonad - an onion of buttons."
    )

-- | Equip a parser with version information about the program
versioner :: Parser (a -> a)
versioner = infoOption (showVersion version <> ", commit " <> fromMaybe "?" $(gitHash))
  (  long "version"
  <> short 'V'
  <> help "Show version"
  )

--------------------------------------------------------------------------------
-- $prs
--
-- The different command-line parsers

-- | Parse the full command
cmdLP :: Parser CmdL
cmdLP = CmdL <$> ccfgP <*> dryrunP
ccfgP :: Parser CCfg
ccfgP =
  CCfg
    <$> iTokenP
    <*> oTokenP
    <*> levelP
    <*> cmpSeqP
    <*> cmpSeqDelayP
    <*> fileP
    <*> fallThrghP
    <*> allowCmdP
    <*> startDelayP
    <*> keySeqDelayP
    <*> implArndP

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

allowCmdP :: Parser (Maybe Bool)
allowCmdP =
  flag' (Just True)
    (  long "allow-cmd"
    <> short 'c'
    <> help "Force allow execution of arbitrary shell-commands"
    )
  <|> flag' (Just False)
    (  long "no-allow-cmd"
    <> help "Force disallow execution of shell-commands"
    )
  <|> pure Nothing

-- | Re-emit unhandled events
fallThrghP :: Parser (Maybe Bool)
fallThrghP =
  flag' (Just True)
    (  long "fallthrough"
    <> short 'f'
    <> help "Force enable fallthrough option to re-emit unhandled events"
    )
  <|> flag' (Just False)
    (  long "no-fallthrough"
    <> help "Force disable fallthrough option. Unhandled events will be discarded."
    )
  <|> pure Nothing


-- | Key to use for compose-key sequences
cmpSeqP :: Parser (Maybe DefButton)
cmpSeqP = optional $ option (megaReadM $ P.buttonP' True)
  (  long "cmp-seq"
  <> short 's'
  <> metavar "BUTTON"
  <> help "Which key to use to emit compose-key sequences"
  )

-- | Specify compose sequence key delays.
cmpSeqDelayP :: Parser (Maybe Int)
cmpSeqDelayP = optional $ option (megaReadM P.numP)
  (  long  "cmp-seq-delay"
  <> metavar "TIME"
  <> help  "How many ms to wait between each key of a compose sequence"
  )

-- | Specify key event output delays.
keySeqDelayP :: Parser (Maybe Int)
keySeqDelayP = optional $ option (megaReadM P.numP)
  (  long  "key-seq-delay"
  <> metavar "TIME"
  <> help  "How many ms to wait between each key event outputted"
  )

-- | How to handle implicit `around`s
implArndP :: Parser (Maybe ImplArnd)
implArndP = optional $ option (megaReadM P.implArndP)
  (  long "implicit-around"
  <> long "ia"
  <> metavar "AROUND"
  <> help "How to translate implicit arounds (`A`, `S-a`)"
  )

-- | Where to emit the output
oTokenP :: Parser (Maybe OToken)
oTokenP = optional $ option (mkTokenP P.otokens)
  (  long "output"
  <> short 'o'
  <> metavar "OTOKEN"
  <> help "Emit output to OTOKEN"
  )

-- | How to capture the keyboard input
iTokenP :: Parser (Maybe IToken)
iTokenP = optional $ option (mkTokenP P.itokens)
  (  long "input"
  <> short 'i'
  <> metavar "ITOKEN"
  <> help "Capture input via ITOKEN"
  )

-- | Parse a flag that disables auto-releasing the release of enter
startDelayP :: Parser Milliseconds
startDelayP = option (fromIntegral <$> megaReadM P.numP)
  (  long  "start-delay"
  <> short 'w'
  <> value 300
  <> metavar "TIME"
  <> showDefaultWith (show . unMS )
  <> help  "How many ms to wait before grabbing the input keyboard (time to release enter if launching from terminal)")

-- | Transform a bunch of tokens of the form @(Keyword, Parser)@ into an
-- optparse-applicative parser
mkTokenP :: [(Text, M.Parser a)] -> ReadM a
mkTokenP = megaReadM . P.mkTokenP' True

-- | Megaparsec <--> optparse-applicative interface
megaReadM :: M.Parser a -> ReadM a
megaReadM p = eitherReader (mapLeft show . M.parse p "" . fromString)
