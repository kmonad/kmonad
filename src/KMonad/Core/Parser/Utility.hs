{-# OPTIONS_HADDOCK not-home #-}
{-|
Module      : KMonad.Core.Parser.Utility
Description : The basic definitions of our mini-language
Copyright   : (c) David Janssen, 2019
License     : MIT
Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : portable

This module defines the basic types of our parser, how we deal with whitespace,
what types of comments we allow, etc.

-}
module KMonad.Core.Parser.Utility
  ( -- * Basic types
    -- $types
    parseE
  , Parser
  , PError

    -- * Basic language definitions
    -- $basic
  , sc, scn
  , lexeme, lexemeSameLine
  , name, symbol, only
  , indent, end, linuxFP
  , someString

    -- * A-list utilities
    -- $alist
  , Name, Named
  , fromNamed

    -- * Reexports
  , Text
  , module Text.Megaparsec
  , module Text.Megaparsec.Char
  , module KMonad.Core.Parser.Token
  )
where

import Control.Monad (void)
import Data.Foldable
import Data.Function (on)
import Data.List (sortBy)
import Data.Text (Text)
import Data.Void (Void)

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import KMonad.Core.Parser.Token

import qualified Data.Set     as S
import qualified Data.Text    as T


--------------------------------------------------------------------------------
-- $types

-- | The narrowing of the 'Parsec' type to our specification
type Parser a = Parsec Void Text a

-- | The error corresponding to our 'Parser' type
type PError = ParseErrorBundle Text Void

-- | 'Text.Megaparsec.parse' but specified to our 'Parser' type.
-- TODO: Deal with errors better: rewrap into ConfigError
parseE :: Parser a -> Text -> a
parseE p t = case parse p "" t of
  (Left e)  -> error $ errorBundlePretty e
  (Right x) -> x


--------------------------------------------------------------------------------
-- $basic

-- | 'failure', but specific for our Parser
failP :: Parser a
failP = failure Nothing S.empty

-- | Consume any combination of tabs, spaces, or "//"-style comments. Does not
-- consume newlines.
sc :: Parser ()
sc = L.space
  (void $ some indent)
  (L.skipLineComment "//")
  failP

-- | Consume any combination of tabs, spaces, line-endings, "//"-style line
-- comments, or "/*...*/" style blockcomments.
scn :: Parser ()
scn = L.space
  space1
  (L.skipLineComment "//")
  (L.skipBlockComment "/*" "*/")

-- | We define indentation as spaces or tabs
-- TODO: Should we support tabs? Does it even work with matrices?
indent :: Parser ()
indent = void (char ' ' <|> char '\t')

-- | Run the parser, then parse as much 'scn' as you can.
lexeme :: Parser a -> Parser a
lexeme = L.lexeme scn

-- | Run the parser, then parse as much 'sc' as you can.
lexemeSameLine :: Parser a -> Parser a
lexemeSameLine = L.lexeme sc

-- | Match exactly the provided Symobl
symbol :: Symbol -> Parser Text
symbol = L.symbol scn

-- | Parse any number of sequential alpha-num chars as a name
name :: Parser Text
name = T.pack <$> (many $ alphaNumChar <|> punctuationChar <|> symbolChar)

-- | Parse anything between ""'s, can't escape " yet
someString :: Parser Text
someString = try $ T.pack <$> (char '"' *> manyTill anySingle (char '"'))

-- | Run the parser IFF it is followed by a space or eof. If that is not the
-- case, the parse state is not updated.
only :: Parser a -> Parser a
only p = try $ p <* (lookAhead $ void spaceChar <|> eof)

-- | Parse either an 'eol' or 'eof'
end :: Parser ()
end = (eof <|> void eol)

-- | Parse a linux-style 'FilePath'
linuxFP :: Parser FilePath
linuxFP = some (alphaNumChar <|> char '/' <|> char '-' <|> char '_' <|> char '.')


--------------------------------------------------------------------------------
-- $alist

-- | We define parser-names as 'Text'
type Name    = Text

-- | A 'Named' value is an alist of names and items
type Named a = [(Name, a)]

-- | Return the ordering of 2 names, longer-names are 'LT'. If names have the
-- same length, then compare alphabetically.
cmpName :: Name -> Name -> Ordering
cmpName a b = case compare (T.length b) (T.length a) of
          EQ -> compare a b
          x  -> x

-- | Sort an AList by length of key, descending, and then alphabetically
sortNamed :: Named a -> Named a
sortNamed = sortBy (cmpName `on` fst)

-- | Turn an Alist of (Pattern, Object) matches into a 'Parser' of 'only'
-- matches. If the provided Alist has duplicate keys, return a set of all
-- duplicate names.
fromNamed :: Named a -> Parser a
fromNamed as = case sortNamed as of
  [] -> failP
  xs -> let dups = findDuplicates (map fst xs) in
    if S.null dups
      then choice . map mkOne $ xs
      else error $ "Duplicate keys: " <> show dups
  where
    mkOne (s, a) = a <$ (only $ string s)


-- | Return a set of all elements in f that occur more than once
findDuplicates :: (Foldable f, Ord a) => f a -> S.Set a
findDuplicates xs = go (toList xs) S.empty S.empty
  where
    go []     old dups = dups
    go (x:xs) old dups = if S.member x old
       then go xs (S.insert x old) (S.insert x dups)
       else go xs (S.insert x old) dups


    -- f ((k, v):xs') mRef = do
    --   m <- readSTRef mRef
    --   if M.member k m
    --     then return . Left $ k
    --     else modifySTRef' mRef (M.insert k v) >> f xs' mRef
