{-|
Module      : KMonad.App
Description : The central app-loop of KMonad
Copyright   : (c) David Janssen, 2019
License     : MIT
Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : portable
-}
module KMonad.App
  (
  )
where

import KPrelude

import KMonad.Keyboard.IO


--------------------------------------------------------------------------------
-- $cfg

data KCfg = KCfg
  { _keySinkDev   :: Acquire KeySink
  , _keySourceDev :: Acquire KeySource
  , _keymapCfg    :: Kh.Keymap Button
  , _firstLayer   :: LayerTag
  , _port         :: Sv.Port
  }
