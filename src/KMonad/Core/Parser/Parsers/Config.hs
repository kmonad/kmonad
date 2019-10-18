{-|
Module      : KMonad.Core.Parser.Parsers.Config
Description : Now to parse the entirety of a configuration file
Copyright   : (c) David Janssen, 2019
License     : MIT
Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : portable

-}
module KMonad.Core.Parser.Parsers.Config
  ( configP
  )
where

import Data.Foldable (foldl')

import KMonad.Core.Parser.Parsers.Alias
import KMonad.Core.Parser.Parsers.IO
import KMonad.Core.Parser.Parsers.Layer
import KMonad.Core.Parser.Parsers.Source
import KMonad.Core.Parser.Utility


--------------------------------------------------------------------------------

-- | Wrap the different possible top-level tokens in a 'Section' ADT
data Section
  = SSource SourceToken
  | SLayer  LayerToken
  | SAlias  AliasDef
  | SInput  InputToken
  | SOutput OutputToken
  deriving (Eq, Show)

-- | Parse a 'Section' by succeeding on any of the top-level definitions
section :: Parser Section
section = lexeme $ choice
  [ SSource <$> sourceP
  , SLayer  <$> layerP
  , SAlias  <$> aliasDefP
  , SInput  <$> inputP
  , SOutput <$> outputP
  ] <* scn

-- | Parse an entire file and return all the different top-level definitions
-- inside a 'ConfigToken' record.
configP :: Parser ConfigToken
configP = scn *> (toCfg . sepSections <$> some section) <* eof
  where toCfg (a, b, c, d, e) = ConfigToken a b c d e

-- | Seperate all the different sections into their own list of definitions.
-- NOTE: This could probably be done *much* prettier, maybe using lenses?
sepSections :: [Section] -> ([SourceToken], [LayerToken], [AliasDef], [InputToken], [OutputToken])
sepSections ss' = (foldl' f ([], [], [], [], []) ss')
  where f (ss, ls, as, is, os) (SSource s) = (s:ss, ls, as, is, os)
        f (ss, ls, as, is, os) (SLayer  l) = (ss, l:ls, as, is, os)
        f (ss, ls, as, is, os) (SAlias  a) = (ss, ls, a:as, is, os)
        f (ss, ls, as, is, os) (SInput  i) = (ss, ls, as, i:is, os)
        f (ss, ls, as, is, os) (SOutput o) = (ss, ls, as, is, o:os)
