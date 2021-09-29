module KMonad.App.Invocation.IO
  ( getInvoc )
where

import Data.Version
import KMonad.Prelude
import KMonad.App.Invocation.Parser
import KMonad.App.Invocation.TH
import KMonad.App.Invocation.Types
import Options.Applicative

import Paths_kmonad (version)


-- | Equip a parser with version information about the program
versioner :: Parser (a -> a)
versioner = infoOption (showVersion version <> ", commit " <> $(gitHash))
  (  long "version"
  <> short 'V'
  <> help "Show version"
  )

-- | Parse 'Invoc' from the evocation of this program
getInvoc :: OnlyIO Invoc
getInvoc = customExecParser (prefs showHelpOnEmpty) $
  info (invocP <**> versioner <**> helper)
    (  fullDesc
    <> progDesc "Start KMonad"
    <> header   "kmonad - an onion of buttons."
    )

