module KMonad.App.Invocation.IO
  ( getInvoc )
where

import KMonad.Prelude
import KMonad.App.Invocation.Types
import KMonad.App.Invocation.Parser

import Options.Applicative

-- | Parse 'Invoc' from the evocation of this program
getInvoc :: IO m => m Invoc
getInvoc = customExecParser (prefs showHelpOnEmpty) $
  info (cmdP <**> versioner <**> helper)
    (  fullDesc
    <> progDesc "Start KMonad"
    <> header   "kmonad - an onion of buttons."
    )

-- | Equip a parser with version information about the program
versioner :: Parser (a -> a)
versioner = infoOption (showVersion version <> ", commit " <> $(gitHash))
  (  long "version"
  <> short 'V'
  <> help "Show version"
  )
