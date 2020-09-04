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

import KMonad.Prelude

import Options.Applicative


--------------------------------------------------------------------------------
-- $cmd
--
-- The different things KMonad can be instructed to do.

-- | Record describing the instruction to KMonad
data Cmd = Cmd
  { _cfgFile :: FilePath -- ^ Which file to read the config from
  , _dryRun  :: Bool     -- ^ Flag to indicate we are only test-parsing
  , _logLvl  :: LogLevel -- ^ Level of logging to use
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
cmdP = Cmd <$> fileP <*> dryrunP <*> levelP

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

