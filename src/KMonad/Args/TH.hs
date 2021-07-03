{-# LANGUAGE BlockArguments #-}
{-|
Module      : KMonad.Args.TH
Description : Template Haskell to use in the CLI
Copyright   : (c) slotThe, 2021
License     : MIT

Maintainer  : soliditsallgood@mailbox.org
Stability   : experimental
Portability : non-portable (TH)

-}
module KMonad.Args.TH (gitHash) where

import KMonad.Prelude

import Language.Haskell.TH (Exp, Q)
import Language.Haskell.TH.Syntax (runIO)
import UnliftIO.Process (readProcessWithExitCode)


-- | Get the git hash of the current revision at compile time
gitHash :: Q Exp
gitHash = do
  str <- runIO do
    (exitCode, hash, _) <- readProcessWithExitCode "git" ["rev-parse", "HEAD"] ""
    pure case exitCode of
      ExitSuccess -> takeWhile (/= '\n') hash
      _           -> ""
  [| fromString str |]
