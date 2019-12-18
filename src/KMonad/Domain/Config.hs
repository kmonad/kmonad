module KMonad.Domain.Config

where

import KMonad.Prelude

import Network.Socket (ServiceName)

import KMonad.Domain.KeyIO


data MapCfg = MapCfg
  { _mcMaps :: !()
  }
makeClassy ''MapCfg

data KCfg = KCfg
  { _kcOpenKeySource :: !(IO KeySource)
  , _kcOpenKeySink   :: !(IO KeySink)
  , _kcPort          :: !ServiceName
  , _kcMapCfg        :: !MapCfg
  }
makeClassy ''KCfg

instance HasMapCfg KCfg where
  mapCfg = kcMapCfg

data KApp = KApp
  { _kaKcfg      :: !KCfg
  , _kaKeySink   :: !KeySink
  , _kaKeySource :: !KeySource
  }
