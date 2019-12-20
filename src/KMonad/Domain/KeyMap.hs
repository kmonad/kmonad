{-# LANGUAGE OverloadedLists #-}
module KMonad.Domain.KeyMap


where

import KMonad.Prelude

import KMonad.Core
import KMonad.Domain.Types

type KeyMap = MapStack Name Keycode Button

type KeyList = ()
