module KMonad.ButtonDocSpec (spec) where

import KMonad.Args.Parser (keywordButtons)
import KMonad.Prelude

import Data.Char (isSpace)
import qualified RIO.Text as T

import Test.Hspec

spec :: Spec
spec = do
  (describe "button-docs" . traverse_ checkForDocsIn)
    [ "doc/quick-reference.md"
    , "keymap/tutorial.kbd"
    ]
 where
  checkForDocsIn file = do
    cnt <- runIO $ readFileUtf8 file
    describe file . sequence_ $ traverse checkForDoc keywordButtons (lines cnt)
  checkForDoc btn cnt =
    it ("Doc mentions " <> unpack (fst btn)) $
      unless (buttonDocExist btn cnt) (expectationFailure "button is not mentioned in documentation")
  buttonDocExist = any . checkerFromButton . fst
  -- ignore deprecated names
  checkerFromButton "momentary-layer" = const True
  checkerFromButton "permanent-layer" = const True
  checkerFromButton btn =
    T.isPrefixOf ("- `(" <> btn <> " ") . T.dropWhile isSpace
      <||> T.isInfixOf ("`" <> btn <> "`")
      <||> T.isInfixOf ("`" <> btn <> "'")
  (<||>) = liftA2 (||)
  infixr 2 <||>
