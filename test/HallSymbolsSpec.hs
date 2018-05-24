module HallSymbolsSpec where

import Test.Hspec
-- import Test.Hspec.QuickCheck
-- import Test.QuickCheck hiding ((.&.))
import Control.Exception (evaluate)
import Text.ParserCombinators.Parsec
import Crystallography.HallSymbols
import Data.Matrix

spec :: Spec
spec = do
  describe "Crystallography.HallSymbols.hallSymbols" $ do
    return ()

  describe "Crystallography.HallSymbols.fromHallSymbols" $ do
    it "[jpn] P 1 wo parse suru to identity ni naru" $ do
      fromHallSymbols "P 1" `shouldBe` (Right [identity 4])

    it "[jpn] P_1 wo parse suru to identity ni naru" $ do
      fromHallSymbols "P_1" `shouldBe` (Right [identity 4])

  describe "Crystallography.HallSymbols.fromHallSymbols'" $ do

    it "throws an exception if used with an empty string" $ do
      evaluate (fromHallSymbols' "") `shouldThrow` anyException

    it "throws an exception if used with an wrong string" $ do
      evaluate (fromHallSymbols' "PP") `shouldThrow` anyException
