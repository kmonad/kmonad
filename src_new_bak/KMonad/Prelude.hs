{-|
Module      : KMonad.Prelude
Description : Code that will be imported into every module
Copyright   : (c) David Janssen, 2019
License     : MIT

Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : non-portable (MPTC with FD, FFI to Linux-only c-code)
-}

module KMonad.Prelude
  ( module X
  )
where

import Control.Arrow   as X ((&&&), (***))
import Control.Lens    as X
import Data.Acquire    as X

import RIO as X hiding
  (-- Not the lens stuff, I want more support for lenses
    view, ASetter, ASetter', Lens, Getting, Lens'
  , SimpleGetter, lens, over, set, sets, to, (^.)

    -- Not the chan stuff, I will use unagi-chan instead
  , newChan, writeChan, dupChan, readChan

  )
