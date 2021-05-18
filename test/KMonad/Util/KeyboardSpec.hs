module KMonad.Util.KeyboardSpec ( spec ) where


import KMonad.Prelude
import KMonad.Util.Keyboard

import Test.Hspec
import qualified RIO.HashMap as M
import qualified RIO.Set     as S

spec :: Spec
spec = do
  describe "keycodes" $ do
   
    it "provides some non-overlapping categories of keycode names" $ do
      let s = sum . map S.size $ [ knLetters, knNumbers, knPunct, knMods
                                 , knFKeys, knKeypad, knOther ]
      s `shouldBe` S.size knAll

    it "provide keycodes for each defined keyname" $ do
      let ks = S.fromList $ M.keys keycodeNames
      knAll `S.difference` ks `shouldBe` S.empty

    it "provides 1 and only 1 name per keycode" $ do
      M.size nameKeycodes `shouldBe` M.size keycodeNames
