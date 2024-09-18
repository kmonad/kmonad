module KMonad.KeycodeSpec (spec) where

import KMonad.Keyboard.Keycode (keyNames)
import KMonad.Util.MultiMap as Q
import KMonad.Prelude

import qualified RIO.NonEmpty as N

import Test.Hspec

spec :: Spec
spec = do
  let dups = filter (not . null . N.tail) $ N.groupAllWith snd (keyNames ^.. Q.itemed)

  it "No duplicate keycode names" $
    dups `shouldBe` []
