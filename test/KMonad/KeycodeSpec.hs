module KMonad.KeycodeSpec (spec) where

import KMonad.Keyboard.Keycode
import KMonad.Util.MultiMap as Q
import KMonad.Prelude
import RIO.List (sort)

import qualified KMonad.Keyboard.IO.Mac.Types as Mac (kcMapRaw)
import qualified KMonad.Keyboard.IO.Windows.Types as Win (winCodeKeyCodeMapping)

import qualified RIO.NonEmpty as N

import Test.Hspec

spec :: Spec
spec = do

  it "No duplicate keycode names" $
    dupsWith snd (keyNames ^.. Q.itemed) `shouldBe` []

  describe "MacOS keycodes" $ checkOsMapping Mac.kcMapRaw
  describe "Windows keycodes" $ checkOsMapping Win.winCodeKeyCodeMapping

checkOsMapping :: (Show a, Ord a) => [(a, Keycode)] -> Spec
checkOsMapping m = do
  -- To check for typos, ...
  let difference = filter (uncurry (/=)) $ zip <*> sort $ m
  let m' = fst <$> difference
  let sortedM' = snd <$> difference
  it "Raw list is sorted" $
    m' `shouldBe` sortedM' -- Only shows unsorted indexes when failing

dupsWith :: Ord b => (a -> b) -> [a] -> [NonEmpty a]
dupsWith f = filter (not . null . N.tail) . N.groupAllWith f
