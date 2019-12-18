{-|
Module      : KMonad.Domain.Message.Message
Description : The collection of all messages that can be sent to KMonad
Copyright   : (c) David Janssen, 2019
License     : MIT

Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : non-portable (MPTC with FD, FFI to Linux-only c-code)
-}
module KMonad.Domain.Message.Message
  ( Message(..)
  )
where

import KMonad.Prelude

import Data.Serialize

import KMonad.Core

-- | The different 'Message's that can be sent to KMonad
data Message
  = Shutdown          -- ^ Signal to quit
  | EmitKey KeyAction -- ^ Signal to emit a key action to the OS
  deriving (Eq, Show, Generic)

instance Serialize Message
