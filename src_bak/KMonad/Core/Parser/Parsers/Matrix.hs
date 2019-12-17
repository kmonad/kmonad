{-|
Module      : KMonad.Core.Parser.Parsers.Matrix
Description : How to parse a matrix of parsers
Copyright   : (c) David Janssen, 2019
License     : MIT
Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : portable

-}
module KMonad.Core.Parser.Parsers.Matrix
  ( matrix
  )
where

import Control.Arrow
import Control.Lens
import KMonad.Core.Matrix
import KMonad.Core.Parser.Utility

type Item a = (Coor, a)
type Row  a = [Item a]

-- | Return a single item and its position in the source code
item :: Parser a -> Parser (Item a)
item it = lexemeSameLine $ do
  pos <- (unPos . sourceLine &&& unPos . sourceColumn) <$> getSourcePos
  i <- it
  return (uncurry mkCoor $ pos , i)

-- | Return a row of items
row :: Parser a -> Parser (Row a)
row it = many indent *> some (item it)

-- | Parse a matrix of values by running and recording the locations of the
-- lexemes of the provided parser.
--
-- For example:
--
-- >>> parseE (matrix keycode) txt
-- To parse a 'Matrix KeyCode'
--
-- >>> parseE (matrix buttonP) txt
-- To parse a 'Matrix ButtonToken'
matrix :: Parser a -> Parser (Matrix a)
matrix it = do
  xs <- endBy1 (row it) end
  return $ (concat xs)^.(from matList)
