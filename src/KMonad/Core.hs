{-|
Module      : KMonad.Core
Description : Reexport all of the Core modules
Copyright   : (c) David Janssen, 2019
License     : MIT

Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : non-portable (MPTC with FD, FFI to Linux-only c-code)

This module only functions to reexport everything from 'KMonad.Core', to shorten
import lists.

-}
module KMonad.Core
  ( -- * Reexports
    module KMonad.Core.Button
  , module KMonad.Core.Config
  , module KMonad.Core.Event
  , module KMonad.Core.Keyboard
  , module KMonad.Core.KeyCode
  , module KMonad.Core.MapStack
  , module KMonad.Core.Matrix
  , module KMonad.Core.Pretty
  , module KMonad.Core.Time
  , module KMonad.Core.Types
  )
where

import KMonad.Core.Button
import KMonad.Core.Config
import KMonad.Core.Event
import KMonad.Core.Keyboard
import KMonad.Core.KeyCode
import KMonad.Core.MapStack
import KMonad.Core.Matrix
import KMonad.Core.Pretty
import KMonad.Core.Time
import KMonad.Core.Types
