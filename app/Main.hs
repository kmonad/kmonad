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

import KPrelude
import KMonad.Runner (kmonad)

main :: IO ()
main = kmonad
