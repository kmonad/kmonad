module KMonad.Engine

where

import KMonad.Prelude

import Control.Concurrent.Chan.Unagi

import KMonad.Event
import KMonad.Keymap

--------------------------------------------------------------------------------

data Env = Env
  { _inChan  :: InChan Event
  , _outChan :: MVar (OutChan Event)
  , _keymap  :: MVar Keymap

  }
