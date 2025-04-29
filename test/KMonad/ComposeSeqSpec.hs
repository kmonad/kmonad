module KMonad.ComposeSeqSpec (spec) where

import KMonad.Args.Parser
import KMonad.Args.Types
import KMonad.Keyboard.ComposeSeq
import KMonad.Keyboard.Keycode
import KMonad.Parsing
import KMonad.Prelude
import KMonad.Util

import Test.Hspec

import qualified RIO.Text as T

spec :: Spec
spec = describe "compose-sequences" $ do
  traverse_ checkComposeSeq ssComposed
  noDuplicates
 where
  noDuplicates = describe "No duplicate compose sequence definitions" $ do
    noDuplicates' _1 "Compose sequences"
    noDuplicates' _2 "Characters"
    noDuplicates' _3 "Character names"
  noDuplicates' field desc = it desc $ duplicatesWith (view field) ssComposed `shouldBe` []
  checkComposeSeq (expected, c, name) = describe ("Compose sequence for " <> unpack name) $ do
    let c' = T.singleton c
    let actualSeq = runParser buttonP "" c'
    let expectedSeq = runParser (KComposeSeq <$> some buttonP <* eof) "" expected
    let actualE2E = parseTokens $ "(deflayer <test> " <> c' <> " )"
    let expectedE2E = first ParseError expectedSeq <&> \x -> [KDefLayer (DefLayer "<test>" [LButton x])]
    it "Is compose sequence" $ actualSeq `shouldSatisfy` parsesAsValidComposeSeq
    it "Matches expected" $ actualSeq `shouldBe` expectedSeq
    it "Could parse in E2E" $ actualE2E `shouldBe` expectedE2E
  parsesAsValidComposeSeq (Right (KComposeSeq seq')) = all isSimple seq'
  parsesAsValidComposeSeq _ = False
  isSimple (KEmit _) = True
  isSimple (KAroundImplicit (KEmit KeyLeftShift) (KEmit _)) = True
  isSimple _ = False
