module KMonad.ButtonDocSpec (spec) where

import KMonad.Args.Parser (keywordButtons)
import KMonad.Prelude

import RIO.Text (isInfixOf, isPrefixOf)

import Test.Hspec

spec :: Spec
spec = do
  (describe "button-docs" . traverse_ checkForDocsIn)
    [ "doc/quick-reference.md"
    ]
 where
  checkForDocsIn file = do
    cnt <- runIO $ readFileUtf8 file
    describe file . sequence_ $ traverse checkForDoc keywordButtons (lines cnt)
  checkForDoc btn cnt =
    it ("Doc for " <> unpack (fst btn) <> " exists") $
      unless (buttonDocExist btn cnt) (expectationFailure "button does not exist in documentation")
  buttonDocExist = any . checkerFromButton . fst
  -- ignore deprecated names
  checkerFromButton "momentary-layer" = const True
  checkerFromButton "permanent-layer" = const True
  checkerFromButton btn =
    isPrefixOf ("- `(" <> btn <> " ")
      <||> isInfixOf ("`" <> btn <> "`")
  (<||>) = liftA2 (||)
