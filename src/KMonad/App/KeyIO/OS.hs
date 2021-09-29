{-# LANGUAGE CPP #-}
module KMonad.App.KeyIO.OS
  ( withKeyInput, withKeyOutput )
where

{-

Import the loaders for 'KeyInputCfg' and 'KeyOutputCfg' based on the current OS.

-}

#ifdef linux_HOST_OS
import KMonad.App.KeyIO.Linux
#endif

#ifdef mingw32_HOST_OS
import KMonad.App.KeyIO.Windows
#endif

#ifdef darwin_HOST_OS
import KMonad.App.KeyIO.Mac
#endif
