{-|
Module      : KMonad
Description : The entry-point to KMonad
Copyright   : (c) David Janssen, 2019
License     : MIT

Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : non-portable (MPTC with FD, FFI to Linux-only c-code)

This module simply exposes a single function that is run by the executable.
Primarily to allow stack to generate documentation.

TODO: Put a large README here

-}
module KMonad
  ( -- * Entrypoint for the KMonad executable
    runKMonad
  )
where
import Test
-- import           System.Environment (getArgs)
import           KMonad.Api.App     (startAppIO)
import           KMonad.Api.Args
import           KMonad.Core.Config (loadConfig)


-- | Get the command-line arguments, parse a config, and start the App-loop
--
-- @since 0.1.0
runKMonad :: IO ()
runKMonad = do
  test
  --args <- getArgs
  --cfg  <- loadConfig (configFile args)
  --startAppIO cfg
