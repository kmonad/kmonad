{-|
Module      : KMonad.Core.Parser.Parsers.IO
Description : How to parse input and output tokens
Copyright   : (c) David Janssen, 2019
License     : MIT
Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : portable

-}
module KMonad.Core.Parser.Parsers.IO
  ( inputP
  , outputP
  )
where

import KMonad.Core.Parser.Utility

import qualified Data.Text as T

--------------------------------------------------------------------------------

-- | Correspondence between names and 'InputDecoder' values
decoderNames :: Named InputDecoder
decoderNames =
  [ ("L64", L64)
  ]

-- | Correspondence between names and parsers that return 'InputToken's
inputNames :: Named (Parser InputToken)
inputNames =
  [ ("LINUX_DEVICE", ldevP)
  , ("LL_KEYBOARD_HOOK", pure WindowsLLHook)
  ]

-- | Parse a LinuxDeviceSource
ldevP :: Parser InputToken
ldevP = do
  dc <- lexemeSameLine $ fromNamed decoderNames
  fp <- lexeme $ linuxFP
  return $ LinuxDeviceSource dc fp

-- | Parse an 'InputToken'
inputP :: Parser InputToken
inputP = do
  _ <- symbol "INPUT"
  _ <- symbol "="
  p <- lexemeSameLine $ fromNamed inputNames
  p


--------------------------------------------------------------------------------

-- | Correspondence between names and 'OutputToken' values.
outputNames :: Named (Parser OutputToken)
outputNames =
  [ ("UINPUT_SINK", uinputP)
  , ("SEND_EVENT_SINK", pure WindowsSendEventSink) ]

-- | Parse a UinputToken
uinputP :: Parser OutputToken
uinputP = do
  let f = maybe Nothing (\s -> if T.null s then Nothing else Just s)
  name <- f <$> (optional $ lexemeSameLine someString)
  post <- f <$> (optional $ lexemeSameLine someString)

  pure $ UinputDevice name post

-- | Parse an 'OutputToken'
outputP :: Parser OutputToken
outputP = do
  _ <- symbol "OUTPUT"
  _ <- symbol "="
  p <- lexemeSameLine $ fromNamed outputNames
  p
