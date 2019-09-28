{-|
Module      : Main
Description : The entry-point to KMonad
Copyright   : (c) David Janssen, 2019
License     : MIT

Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : non-portable (MPTC with FD, FFI to Linux-only c-code)

The entry-point to KMonad that simply imports the relevant modules and strings
them together. For the actual implementation details see "KMonad.Api.App"
-}
module Main
  ( -- * The entry-point to KMonad
    main
  )
where

import           System.Environment (getArgs)

import           KMonad.Api.App     (startAppIO)
import           KMonad.Core.Config (loadConfig)



-- | Get the command-line arguments, parse a config, and start the App-loop
--
-- @since 0.1.0
main :: IO ()
main = do
  pth <- head <$> getArgs
  cfg <- loadConfig pth
  startAppIO cfg
