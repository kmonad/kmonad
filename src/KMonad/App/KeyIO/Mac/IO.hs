module KMonad.App.KeyIO.Mac.IO
  ( withKeyInput
  , withKeyOutput
  )
where

import KMonad.Prelude
import KMonad.Util
import KMonad.Util.OS

import KMonad.App.KeyIO.Common
import KMonad.App.KeyIO.Mac.IOKitSource (withIOKitSource)
import KMonad.App.KeyIO.Mac.Ext (withExt)

-- | Load a 'KIOKitCfg' if provided, otherwise throw a platform error
withKeyInput :: LUIO m e => KeyInputCfg -> Ctx r m GetKey
withKeyInput (MacIOKitCfg c) = withIOKitSource c
withKeyInput c = throwIO . osErr $ "unsupported input method " <> show c

-- | Load a 'MacExtCfg' if provided, otherwise throw a platform error
withKeyOutput :: LUIO m e => KeyOutputCfg -> Ctx r m PutKey
withKeyOutput (MacExtCfg c) = withExt c
withKeyOutput c = throwIO . osErr $ "unsupported output method " <> show c
