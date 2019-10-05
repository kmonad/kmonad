{-|
Module      : KMonad.Api.Encode
Description : Translate tokens to their App-equivalents
Copyright   : (c) David Janssen, 2019
License     : MIT
Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : portable

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
import KMonad.Api.KeyIO.Linux.DeviceSource
import KMonad.Api.KeyIO.Linux.UinputSink

import qualified Data.Text as T

--------------------------------------------------------------------------------

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
