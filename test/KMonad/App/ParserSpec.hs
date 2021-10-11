module KMonad.App.ParserSpec ( spec ) where

import KMonad.Prelude
import KMonad.Util.Keyboard
import KMonad.App.Parser hiding (try)
import KMonad.App.Parser.IO
-- import KMonad.App.Parser.TokenJoiner
import KMonad.App.Parser.Keycode

import Test.Hspec
import RIO.Directory
import RIO.FilePath
import qualified RIO.Set     as S
import qualified RIO.HashMap as M

import System.Directory.PathWalk

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

  describe "usermaps" $ do

    (pErrs, jErrs) <- runIO testUsermaps

    it "correctly parses all configurations into KExprs" $ do
      pErrs `shouldBe` []

    it "correctly joins all parsed configurations into full config tokens" $ do
      jErrs `shouldBe` []


testUsermaps :: OnlyIO ([(FilePath, PErrors)], [(FilePath, JoinError)])
testUsermaps = do

  -- Find all '.kbd' files in the 'keymap' directory
  dir <- (</> "keymap") <$> getCurrentDirectory
  let go p _ f = pure $ if takeBaseName p /= "template"
        then map (p </>) . filter (".kbd" `isExtensionOf`) $ f
        else []
  fs <- pathWalkAccumulate dir go

  -- Parse all files, collecting different errors
  let g fname = parseFile fname >>= \res -> pure $ case res of
        Success _ -> []
        PError  p -> [Left  (fname, p)]
        JError  j -> [Right (fname, j)]
  es <- foldMapM g fs
  pure $ partitionEithers es
