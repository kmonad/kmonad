{-|
Module      : KMonad.Core.Parser.Parsers.Layer
Description : How to parse an entire layer
Copyright   : (c) David Janssen, 2019
License     : MIT
Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : portable

-}
module KMonad.Core.Parser.Parsers.Layer
  ( layerP
  )
where

import KMonad.Core.Parser.Parsers.Button
import KMonad.Core.Parser.Parsers.KeyCode
import KMonad.Core.Parser.Parsers.Matrix
import KMonad.Core.Parser.Utility

-- | Parse the header of a 'LayerToken' definition
header :: Parser (Name, Maybe KeyCode)
header = do
  _  <- symbol "LAYER"
  n  <- lexeme name
  kc <- optional $ do
    _ <- symbol "~"
    _ <- symbol "anchor"
    keycodeP
  return (n, kc)

-- | Parse a 'LayerToken'
layerP :: Parser LayerToken
layerP = do
  (nm, an) <- lexeme header
  mat      <- matrix buttonSymbolP
  return $ LayerToken
    { _layerName = nm
    , _anchor    = an
    , _buttons   = mat }
