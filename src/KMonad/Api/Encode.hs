{-# LANGUAGE CPP #-}
{-|
Module      : KMonad.Api.Encode
Description : Translate tokens to their App-equivalents
Copyright   : (c) David Janssen, 2019
License     : MIT
Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : portable

Essentially, everything in KMonad should simply compile normally, regardless of
OS. We use cabal-file conditionals to include/exclude OS-specific code on
different platforms, but we need 1 spot where we determine how to dispatch
encoding based on the operating system. That is *this* module.


-}
module KMonad.Api.Encode
  ( pickInputIO
  , pickOutputIO
  )
where

import Control.Lens
import Data.Maybe

import KMonad.Core.Parser

import KMonad.Api.KeyIO
import qualified Data.Text as T


-- Linux only imports
#ifdef linux_HOST_OS
import KMonad.Api.KeyIO.Linux.DeviceSource
import KMonad.Api.KeyIO.Linux.UinputSink
#endif

#ifdef mingw32_HOST_OS
import System.Win32
#endif

-- Windows only import

-- Linux dispatch

#ifdef linux_HOST_OS

-- | Translate an 'InputToken' to a 'KeySource' object
pickInputIO :: InputToken -> KeySource
pickInputIO (LinuxDeviceSource L64 pth) = deviceSource64 pth

-- | Translate an 'OutputToken' to a 'KeySink' object
pickOutputIO :: OutputToken -> KeySink
pickOutputIO (UinputDevice nm pi) = let def = defUinputCfg in
  mkUinputSink $ def
    { _keyboardName = fromMaybe (def^.keyboardName) (T.unpack <$> nm)
    , _postInit     = T.unpack <$> pi
    }

#endif

#ifdef mingw32_HOST_OS

pickInputIO :: InputToken -> KeySource
pickInputIO _ = error "not implemented yet"

pickOutputIO :: OutputToken -> KeySink
pickOutputIO _ = error "not implemented yet"

#endif
