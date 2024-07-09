module KMonad.ButtonDocSpec (spec) where

import KMonad.Args.Parser
import KMonad.Args.Types
import KMonad.Prelude

import Data.Char (isSpace)
import Data.Data
import qualified RIO.Text as T

import Test.Hspec

spec :: Spec
spec = do
  docsExistForEveryButtons
  tutorialMentionsEveryButton

docsExistForEveryButtons :: Spec
docsExistForEveryButtons =
  (describe "button-docs" . traverse_ checkForDocsIn)
    [ quickReferencePath
    , tutorialPath
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

tutorialMentionsEveryButton :: Spec
tutorialMentionsEveryButton =
  describe "button-usage" $
    sequence_ . traverse checkButtonUsed buttonConstrs =<< runIO getTutorial
 where
  checkButtonUsed btn cnt =
    it ("Buttontype `" <> showConstr btn <> "` appears outside of comments")
      . unless (containsButton btn cnt)
      $ expectationFailure ("Buttontype `" <> showConstr btn <> "` is never used")
  containsButton btn = any . anyButton $ (== btn) . toConstr
  anyButton f (KDefLayer DefLayer{_layerSettings = lyrBtns}) = anyOf (each . _LButton) (anySubButton f) lyrBtns
  anyButton f (KDefAlias als) = any (anySubButton f . snd) als
  anyButton _ _ = False
  anySubButton f x = f x || anyOf plate f x
  buttonConstrs = dataTypeConstrs $ dataTypeOf (undefined :: DefButton)

getTutorial :: IO [KExpr]
getTutorial =
  either cannotParseTutorial pure . parseTokens
    =<< readFileUtf8 tutorialPath
 where
  cannotParseTutorial err =
    fail ("Could not parse `" <> tutorialPath <> "`:\n" <> show err)
      $> []

quickReferencePath :: FilePath
quickReferencePath = "doc/quick-reference.md"
tutorialPath :: FilePath
tutorialPath = "keymap/tutorial.kbd"
