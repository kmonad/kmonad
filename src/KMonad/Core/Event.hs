{-|
Module      : KMonad.Core.Event
Description : The different input events KMonad can deal with.
Copyright   : (c) David Janssen, 2019
License     : MIT

Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : non-portable (MPTC with FD, FFI to Linux-only c-code)

-}
module KMonad.Core.Event
  ( -- * The type and constructors of 'Event'
    Event (..)
  , keyEventAt
  )
where

import KMonad.Core.Keyboard
import KMonad.Core.Time

-- | An 'Event' is either a 'KeyEvent' or a signal to shutdown
data Event
  = InputEvent KeyEvent
  | Quit
  deriving (Eq, Show)

keyEventAt :: KeyAction -> Time -> Event
keyEventAt a = InputEvent . actAtTime a
