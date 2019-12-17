{-|
Module      : KMonad.Prelude
Description : An enumeration of keycodes
Copyright   : (c) David Janssen, 2019
License     : MIT

Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : non-portable (MPTC with FD, FFI to Linux-only c-code)

This contains code that I end up importing in basically every module. At the
moment we import RIO (which itself imports a great many other things). We swap
out the `microlens` stuff from RIO with the full-fledged Control.Lens library
though.

-}

module KMonad.Prelude
  ( module Control.Arrow
  , module Control.Lens
  , module RIO )
where

import Control.Arrow ((&&&), (***))
import Control.Lens
import RIO hiding (view, ASetter, ASetter', Lens, Getting,
                   Lens', SimpleGetter, lens, over, set,
                   sets, to, (^.))
