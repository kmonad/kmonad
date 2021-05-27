module KMonad.App.KeyIO.Windows.IO
  ( withKeyInput
  , withKeyOutput
  )
where

import KMonad.Prelude
import KMonad.Util
import KMonad.Util.OS

import KMonad.App.KeyIO.Common
import KMonad.App.KeyIO.Windows.LowLevelHook (withLowLevelHook)
import KMonad.App.KeyIO.Windows.SendEvent (withSendEvent)

import qualified RIO.List as L

-- | Load a 'LowLevelHook' if provided, otherwise OS error
withKeyInput :: LUIO m e => KeyInputCfg -> Ctx r m GetKey
withKeyInput (WindowsLowLevelHookCfg c) = withLowLevelHook c
withKeyInput c = throwIO . osErr $ "unsupported input method " <> show c

-- | Load a 'UinputCfg' if provided, else throw a platform error
withKeyOutput :: LUIO m e => KeyOutputCfg -> Ctx r m PutKey
withKeyOutput (WindowsSendEventCfg c) = withSendEvent c
withKeyOutput c = throwIO . osErr $ "unsupported output method " <> show c
