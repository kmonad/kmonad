{-|
Module      : KMonad.Domain.Event
Description : All the different events that occur in a KMonad session
Copyright   : (c) David Janssen, 2019
License     : MIT

Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : non-portable (MPTC with FD, FFI to Linux-only c-code)
-}
module KMonad.Domain.Event
  ( Event(..)
  )
where

import KMonad.Prelude

import Data.Serialize

import KMonad.Core
import KMonad.Domain.Message.Message



-- | An 'Event' is either a 'KeyEvent' or a signal to shutdown
data Event
  = InputEvent   KeyEvent
  | MessageEvent Message
  | Quit
  deriving (Eq, Show, Generic)

instance Serialize Event
