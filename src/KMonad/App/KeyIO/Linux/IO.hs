module KMonad.App.KeyIO.Linux.IO
  ( withKeyInput
  , withKeyOutput
  )
where

import KMonad.Prelude
import KMonad.Util
import KMonad.Util.OS

import KMonad.App.KeyIO.Common
import KMonad.App.KeyIO.Linux.Evdev  (withEvdev)
import KMonad.App.KeyIO.Linux.Uinput (withUinput)

-- | Load an 'EvdevCfg' if provided, else throw a platform error
withKeyInput :: LUIO m e => KeyInputCfg -> Ctx r m GetKey
withKeyInput (LinuxEvdevCfg c) = withEvdev c
withKeyInput c = throwIO . osErr $ "unsupported input method " <> show c

-- | Load a 'UinputCfg' if provided, else throw a platform error
withKeyOutput :: LUIO m e => KeyOutputCfg -> Ctx r m PutKey
withKeyOutput (LinuxUinputCfg c) = withUinput c
withKeyOutput c = throwIO . osErr $ "unsupported output method " <> show c
