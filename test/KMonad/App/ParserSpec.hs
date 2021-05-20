module KMonad.App.ParserSpec ( spec ) where

import KMonad.Prelude
import KMonad.Util.Keyboard
import KMonad.App.Parser.Keycode

import Test.Hspec
import qualified RIO.Set     as S

spec :: Spec
spec = do
  describe "aliases" $ do

    let kas = keycodeAliases

    it "only reference valid CoreNames" $ do
      let tgts = S.fromList $ kas ^.. folded . _2 . to CoreName
      tgts `S.difference` knAll `shouldBe` S.empty

    it "none of the aliases overlap eachother" $ do
      let dups = duplicates $ kas ^.. folded . _1
      dups `shouldBe` S.empty

    -- it "none of the aliases overlap CoreNames" $ do
    --   let
