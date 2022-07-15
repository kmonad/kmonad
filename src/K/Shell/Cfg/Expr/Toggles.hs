{- | TODO insert header

TODO insert footer

-}
module K.Shell.Cfg.Expr.Toggles
  ( togglesExpr )
where

import K.Shell.Cfg.Expr.Initial
import K.Gesture (Toggles)

import Data.Char
import qualified RIO.Text as T

-- basic values ----------------------------------------------------------------

-- | An expression relating sequences of 'Toggle Name' to 'Text'
togglesExpr :: Expr (Toggles Name)
togglesExpr = customExpr "Toggles" togglesT togglesP

-- parser ----------------------------------------------------------------------

-- | Parse a sequence of elements into a 'Toggles' of 'Text'
togglesP :: Parser (Toggles Name)
togglesP = fmap mconcat . some . lex . choice $
  [subg, try openTag, try around_, try closeTag, tap_]

-- | Characters that may not occur in tag-names
reserved :: [Char]
reserved = "()-[]"

-- | Parse a series of valid characters as a tag
tag_ :: Parser Text
tag_ = takeWhile1P (Just "tag-character") f
  where f c = not $ isSpace c || c `elem` reserved

-- | Parse a "S-" sequence as 1 tag around another
around_ :: Parser (Toggles Name)
around_ = do
  a <- tag_
  _ <- char '-'
  b <- try around_ <|> subg <|> tap_
  pure $ (On a <| b) |> Off a

-- | Parse a ")-X" as an OFF-toggle
closeTag :: Parser (Toggles Name)
closeTag = do
  _ <- string ")-"
  a <- tag_
  pure [Off a]

-- | Parse a "X-(" as an ON-toggle
openTag :: Parser (Toggles Name)
openTag = do
  a <- tag_
  _ <- string "-("
  pure [On a]

-- | Parse only a tag as a tap of that element
tap_ :: Parser (Toggles Name)
tap_ = do
  a <- tag_
  pure $ [On a, Off a]

-- | Parse a [] delimited series as a nested gesture
subg :: Parser (Toggles Name)
subg = do
  _ <- char '['
  g <- togglesP
  _ <- char ']'
  pure g

-- printer ---------------------------------------------------------------------

-- | Print a foldable of toggling names.
togglesT :: Foldable f => Printer (f (Toggle Name))
togglesT = T.intercalate " " . reverse . go [] . toList
  where
    go :: [Text] -> [Toggle Name] -> [Text]
    go acc [] = acc
    go acc ((On a):(Off b):rst) | a == b = go (a:acc) rst
    go acc (On a:rst) = go ((a <> "-("):acc) rst
    go acc (Off a:rst) = go ((")-" <> a):acc) rst
