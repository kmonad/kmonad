module KMonad.GestureSpec ( spec ) where

import KMonad.Prelude
import KMonad.Gesture

import Data.Either (fromRight)

import Test.Hspec hiding (around)

r :: Either a b -> b
r = fromRight undefined

spec :: Spec
spec = do

  let abc  = tap "a" <> tap "b" <> tap "c"
  let sa   = r $ around "S" (tap "a")
  let csa  = r $ around "C" sa
  let c    = tap "C"
  let sabc = r $ around "S" abc
  let xx   = r $ around "C" (sa <> tap "b")

  describe "gesture" $ do

    it "parses \"a b c\" as series of taps" $ do
      readGesture "a b c" `shouldBe` Right abc

    it "parses \"S-a\" as S held around tap of a" $ do
      readGesture "S-a" `shouldBe` Right sa

    it "parses \"C-S-a\" as C around S around a" $ do
      readGesture "C-S-a" `shouldBe` Right csa

    it "parses \"C-(  )-C\" as the press and release of C" $ do
      readGesture "C-( )-C" `shouldBe` Right c

    it "parser \"C-(S-a b)-C\" as C around shifted-a b" $ do
      readGesture "C-(S-a b)-C" `shouldBe` Right xx

    it "parses \"S-[a b c]\" as S around taps of a b c" $ do
      readGesture "S-[a b c]" `shouldBe` Right sabc
