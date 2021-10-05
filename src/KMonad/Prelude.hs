{-# OPTIONS_GHC -Wno-dodgy-imports #-}
{-|
Module      : KMonad.Prelude
Description : Code that will be imported into every module
Copyright   : (c) David Janssen, 2019
License     : MIT

Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : non-portable (MPTC with FD, FFI to Linux-only c-code)
-}

module KMonad.Prelude ( module X ) where

import KMonad.Prelude.Imports as X
import KMonad.Prelude.Types   as X
import KMonad.Prelude.Util    as X
