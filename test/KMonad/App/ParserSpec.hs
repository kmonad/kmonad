module KMonad.App.ParserSpec ( spec ) where

import KMonad.Prelude
import KMonad.Util.Keyboard
import KMonad.App.Parser.Keycode

import Test.Hspec
import qualified RIO.Set     as S
import qualified RIO.HashMap as M

spec :: Spec
spec = do
  describe "aliases" $ do

    -- Set of all alias-names we use
    let als = S.fromList $ keycodeAliases ^.. folded . _1
    -- Set of unwrapped corenames we use
    let crs = S.fromList $ (M.keys keycodeNames) ^.. folded . to unCore


    it "only reference valid CoreNames" $ do
      let tgts = S.fromList $ keycodeAliases ^.. folded . _2
      tgts `S.difference` crs `shouldBe` S.empty

    it "none of the aliases overlap eachother" $ do
      let dups = duplicates als
      dups `shouldBe` S.empty

    it "none of the aliases overlap CoreNames" $ do
      als `S.intersection` crs `shouldBe` S.empty
