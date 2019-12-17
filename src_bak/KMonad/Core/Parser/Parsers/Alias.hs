{-|
Module      : KMonad.Core.Parser.Parsers.Alias
Description : How to parse alias definitions
Copyright   : (c) David Janssen, 2019
License     : MIT
Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : portable

We parse assignment to an \@-prefixed name as an alias.

>>> @hello = e

This defined /hello/ as a button that emits /e/ characters.

-}
module KMonad.Core.Parser.Parsers.Alias
  ( -- * Alias definition
    aliasDefP

    -- | We export aliasRefP, but define it in
    -- "KMonad.Core.Parser.Parser.Button" due to circular-import reasons.
  , aliasRefP
  )
where

import KMonad.Core.Parser.Parsers.Button
import KMonad.Core.Parser.Utility


--------------------------------------------------------------------------------

-- | Parse an alias definition
aliasDefP :: Parser AliasDef
aliasDefP = do
  _ <- char '@'
  s <- lexeme name
  _ <- lexeme $ char '='
  b <- buttonP
  return $ AliasDef s b

