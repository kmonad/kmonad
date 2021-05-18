{-# LANGUAGE CPP #-}
module KMonad.Util.OS

where

import KMonad.Prelude

-- Utilities for managing cross-platform support

--------------------------------------------------------------------------------
-- $os

-- | An ADT of all platforms that we support
data OS = Linux | Mac | Windows deriving (Eq, Show)

-- | The OS on which KMonad is currently running
currentOS :: OS
#ifdef linux_HOST_OS
currentOS = Linux
#endif
#ifdef darwin_HOST_OS
currentOS = Mac
#endif
#ifdef mingw32_HOST_OS
currentOS = Windows
#endif

--------------------------------------------------------------------------------
-- $err

-- | Exception indicating an attempt to do something unsupported on this OS
data WrongOSException = WrongOSException OS String deriving Show
instance Exception WrongOSException where
  displayException (WrongOSException os t) = show os <> " error: " <> t

osErr :: String -> WrongOSException
osErr = WrongOSException currentOS
