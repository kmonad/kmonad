module KMonad.App.Invocation.IO
  ( getInvoc )
where

import KMonad.Prelude
import KMonad.App.Invocation.Types
import KMonad.App.Invocation.Parser

import Options.Applicative

-- | Parse 'Invoc' from the evocation of this program
getInvoc :: IO Invoc
getInvoc = customExecParser (prefs showHelpOnEmpty) $ info (invocP <**> helper)
  (  fullDesc
  <> progDesc "Start KMonad"
  <> header   "kmonad - an onion of buttons."
  )
