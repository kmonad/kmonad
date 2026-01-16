module KMonad.ComposeSeqSpec (spec) where

import KMonad.Args.Parser
import KMonad.Args.Types
import KMonad.Keyboard.ComposeSeq
import KMonad.Keyboard.Keycode
import KMonad.Parsing
import KMonad.Prelude
import KMonad.Util

import Test.Hspec

import RIO.List (repeat)
import qualified RIO.Text as T

spec :: Spec
spec = describe "compose-sequences" $ do
  validSeqs
  noDuplicates
 where
  noDuplicates = describe "No duplicate compose sequence definitions" $ do
    noDuplicates' _1 "Compose sequences"
    noDuplicates' _2 "Characters"
    noDuplicates' _3 "Character names"
  noDuplicates' field desc = it desc $ duplicatesWith (view field) ssComposed `shouldBe` []
  validSeqs = describe "Compose sequences are valid" $
    case foldr (zipWith (++)) (repeat []) $ checkComposeSeq <$> ssComposed of
      [notCmpSeqs, notAsExpected, notAsExpectedE2E] -> do
        it "All are compose sequences" $ notCmpSeqs `shouldBe` []
        it "All match as expected" $ notAsExpected `shouldBe` []
        it "All parse in E2E" $ notAsExpectedE2E `shouldBe` []
      _ -> error "Invalid number of conditions in compose sequence checks"

  checkComposeSeq :: (Text, Char, Text) -> [[Text]]
  checkComposeSeq (expected, c, name) =
    [ parsesAsValidComposeSeq actualSeq
    , actualSeq == expectedSeq
    , actualE2E == expectedE2E
    ] <&> bool [name] []
   where
    actualSeq = runParser buttonP "" c'
    expectedSeq = runParser (KComposeSeq <$> some buttonP <* eof) "" expected
    actualE2E = parseTokens $ "(deflayer <test> " <> c' <> " )"
    expectedE2E = first ParseError expectedSeq <&> \x -> [KDefLayer (DefLayer "<test>" [LButton x])]
    c' = T.singleton c

  parsesAsValidComposeSeq (Right (KComposeSeq seq')) = all isSimple seq'
  parsesAsValidComposeSeq _ = False
  isSimple (KEmit _) = True
  isSimple (KAroundImplicit (KEmit KeyLeftShift) (KEmit _)) = True
  isSimple _ = False
