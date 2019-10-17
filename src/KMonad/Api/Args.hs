module KMonad.Api.Args

where

import Control.Lens hiding (argument)
import Options.Applicative
import Text.Read

import KMonad.Core
import KMonad.Core.Parser.Token



data CmdLineArgs = CmdLineArgs
  { _configFile   :: FilePath
  -- , restartDelay :: Maybe Milliseconds
  , _inputDevice  :: Maybe InputToken
  -- , outputDevice :: Maybe OutputToken
  }
makeLenses ''CmdLineArgs

getArgs :: IO CmdLineArgs
getArgs = execParser opts
  where
    opts = info (allArgs <**> helper)
      ( fullDesc
     <> progDesc "Start KMonad"
     <> header   "KMonad - an advanced keyboard utility")

allArgs :: Parser CmdLineArgs
allArgs = CmdLineArgs <$> configFileA <*> inputDeviceA

configFileA :: Parser FilePath
configFileA = argument str $
    metavar "FILE"
 <> help    "The configuration file to load"

--------------------------------------------------------------------------------

-- -- | Argument for the restart delay
-- restartDelayA :: Parser (Maybe Milliseconds)
-- restartDelayA = option toRestartDelayA $
--     long    "delay"
--  <> short   'd'
--  <> metavar "MILLISECONDS"
--  <> value   (Just 1000)
--  <> help    (concat [ "How long to wait before attempting to restart after an IO error. "
--                     , "Or 0 for no attempt at restarting" ])

-- -- | Parse a restart-delay from a string
-- toRestartDelayA :: ReadM (Maybe Milliseconds)
-- toRestartDelayA = maybeReader $ \s -> case (readMaybe s :: Maybe Milliseconds) of
--   Nothing -> Nothing
--   Just 0  -> Just Nothing
--   Just t  -> Just (Just t)

-- --------------------------------------------------------------------------------

-- | Argument for optional input device
inputDeviceA :: Parser (Maybe InputToken)
inputDeviceA = option toInputDeviceA $
    long "input"
 <> short 'i'
 <> metavar "KEY_INPUT"
 <> value Nothing
 <> help "Optionally provide the input device to capture"

toInputDeviceA :: ReadM (Maybe InputToken)
toInputDeviceA = maybeReader $ \s -> Just (Just (LinuxDeviceSource L64 s))


-- outputDeviceA :: Parser (Maybe OutputToken)
-- outputDeviceA = toOutputDeviceA <$> strOption (
--     long "output"
--  <> short 'o'
--  <> metavar "KEY_OUTPUT"
--  <> value ""
--  <> help "Optionally provide the output device to capture"
--  )

-- toOutputDeviceA :: String -> Maybe OutputToken
-- toOutputDeviceA _ = Nothing


-- foo :: Parser String
-- foo = undefined
