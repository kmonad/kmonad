module KMonad.ComposeSeqSpec (spec) where

import KMonad.Args.Parser
import KMonad.Args.Types
import KMonad.Keyboard.ComposeSeq
import KMonad.Keyboard.Keycode
import KMonad.Parsing
import KMonad.Prelude

import Test.Hspec

spec :: Spec
spec = describe "compose-sequences" $ traverse_ checkComposeSeq ssComposed
 where
  checkComposeSeq (_, c, name) =
    it ("Compose sequence for " <> unpack name <> " is valid") $
      runParser buttonP "" (pack [c]) `shouldSatisfy` parsesAsValidComposeSeq
  parsesAsValidComposeSeq (Right (KComposeSeq seq')) = all isSimple seq'
  parsesAsValidComposeSeq _ = False
  isSimple (KEmit _) = True
  isSimple (KAround (KEmit KeyLeftShift) (KEmit _)) = True
  isSimple _ = False
