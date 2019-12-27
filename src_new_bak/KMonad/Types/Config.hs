module KMonad.Types.Config

where

import KMonad.Prelude
import KMonad.Types.Message

-- The types of configs used in multiple locations in KMonad

data NetworkCfg = NetworkCfg
  { _port :: Port
  }

