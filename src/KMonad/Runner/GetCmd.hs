module KMonad.Runner.GetCmd
  ( RunCfg(..)
  , LogTarget(..)
  , Command(..)
  , getRunCfg
  )
where

import KPrelude

import Options.Applicative
import KMonad.Runner.Types



--------------------------------------------------------------------------------
-- $cmd

-- type Port = ()
-- type Message = Text


--------------------------------------------------------------------------------
-- $run
--

-- | Parse 'RunCfg' from the evocation of this program
getRunCfg :: IO RunCfg
getRunCfg = customExecParser (prefs showHelpOnEmpty) $ info (envP <**> helper)
  (  fullDesc
  <> progDesc "Start or instruct KMonad"
  <> header   "kmonad - the onion of keyboard management."
  <> footer ( "NOTE: args for a subcommand must be given directly after it" )
  )

-- | Parse an entire 'RunCfg'
envP :: Parser RunCfg
envP = RunCfg <$> cmdP <*> levelP <*> handleP

-- | Parse the specific KMonad action to undertake
cmdP :: Parser Command
cmdP = hsubparser
     ( command "start" (info (StartDaemon <$> startDaemonCmd)
                       ( progDesc "Start KMonad"
                      <> header   "kmonad - the onion of keyboard management."))
   <> command "test"   (info (TestConfig <$> testConfigCmd)
                       ( progDesc "Test configuration"
                      <> header   "kmonad - the onion of keyboard management."))
     )

--------------------------------------------------------------------------------
-- $strt

-- | Parse the command to start then daemon
startDaemonCmd :: Parser StartDaemonCmd
startDaemonCmd = StartDaemonCmd
  <$> strArgument     ( metavar "FILE"
                     <> help    "The configuration file to load.")

-- | Parse the log-level as either a level option or a verbose flag
levelP :: Parser LogLevel
levelP =
      option f
         ( long    "log-level"
        <> short   'l'
        <> metavar "Log level"
        <> value   LevelWarn
        <> help    "How much info to print out (debug, info, warn, error). Don't mix with -v" )
  <|> flag' LevelDebug
         ( long    "verbose"
        <> short   'v'
        <> help    "Set verbosity to 'debug'. Don't mix with -l" )
  where
    f = maybeReader $ flip lookup [ ("debug", LevelDebug), ("warn", LevelWarn)
                                  , ("info",  LevelInfo),  ("error", LevelError) ]

-- | When a file is passed, set 'LogTarget' to that file.
handleP :: Parser LogTarget
handleP =
  option (ToFile <$> str)
     ( long    "log-file"
    <> short   'f'
    <> metavar "Log file"
    <> value   Stdout
    <> help    "Specify a file to log to (default: stdout)")

--------------------------------------------------------------------------------
-- $tst

-- | Parse the command to test a configuration
testConfigCmd :: Parser TestConfigCmd
testConfigCmd = TestConfigCmd
  <$> strArgument ( metavar "FILE"
                 <> help    "The configuration file to test")
