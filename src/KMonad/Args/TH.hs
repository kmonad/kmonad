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

import Control.Monad.Trans.Maybe
import Language.Haskell.TH (Exp, Q, runIO)
import UnliftIO.Directory (findExecutable)
import UnliftIO.Process (readProcessWithExitCode)

-- | Get the git hash of the current commit at compile time.
gitHash :: Q Exp
gitHash = do
  -- This makes use of the `MonadFail` instance for `MaybeT`,
  -- which simply returns `Nothing` on failure.
  hash <- runIO . runMaybeT $ do
    Just git <- lift $ findExecutable "git"
    (ExitSuccess, hash, _) <- lift $ readProcessWithExitCode git ["rev-parse", "HEAD"] ""
    pure $ takeWhile (/= '\n') hash
  [| fromString <$> hash |]
