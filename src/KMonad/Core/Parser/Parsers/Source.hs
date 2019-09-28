{-|
Module      : KMonad.Core.Parser.Parsers.Source
Description : How to parse a source token of keycodes
Copyright   : (c) David Janssen, 2019
License     : MIT
Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : portable

-}
module KMonad.Core.Parser.Parsers.Source
  ( sourceP
  )
where

import KMonad.Core.Parser.Parsers.KeyCode
import KMonad.Core.Parser.Parsers.Matrix
import KMonad.Core.Parser.Utility

-- | Parse a 'SourceToken'
sourceP :: Parser SourceToken
sourceP = symbol "SRC" >> SourceToken <$> matrix keycodeP
