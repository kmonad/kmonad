{-|
Module      : Main
Description : The entry-point to KMonad
Copyright   : (c) David Janssen, 2019
License     : MIT

Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : non-portable (MPTC with FD, FFI to Linux-only c-code)

-}
module Main
  ( -- * The entry-point to KMonad
    main
  )
where

import KPrelude
import KMonad.Args.Cmd
import KMonad.App

main :: IO ()
main = getCmd >>= print
