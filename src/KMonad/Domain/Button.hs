{-|
Module      : KMonad.Domain.Button
Description : Entrypoint for all effectful Button functionality
Copyright   : (c) David Janssen, 2019
License     : MIT

Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : non-portable (MPTC with FD, FFI to Linux-only c-code)

This module reexports all of the different 'Button' implementations from the
"KMonad.Domain.Button" submodule, and provides a function that translates a
'KMonad.Core.Parser.Token.ButtonToken' to a concrete implementation. For
anything that uses 'Button's, it should be sufficient to simply import this
module.

-}
module KMonad.Domain.Button
  ( module KMonad.Domain.Button.Button
  , module KMonad.Domain.Button.Buttons
  , module KMonad.Domain.Button.Encode
  )
where

import KMonad.Domain.Button.Button
import KMonad.Domain.Button.Buttons
import KMonad.Domain.Button.Encode
