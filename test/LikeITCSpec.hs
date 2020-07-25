module LikeITCSpec where

import Test.Hspec

import Control.Exception (evaluate)
import Text.ParserCombinators.Parsec
import Crystallography.HallSymbols
import Crystallography.HallSymbols.LikeITC
import Crystallography.HallSymbols.SpacegroupSymbols
import Data.Maybe
import Data.List
import Data.Matrix
import Data.Matrix.AsXYZ

-- for check about equivalent
sort' :: Ord a => [Matrix a] -> [[a]]
sort' xs = sort . map toList $ xs

spec :: Spec
spec = do
   describe "symmetryOperations" $ do
     mapM_ testSymmetryOperations $ map fst generatorsSelectedTable

testSymmetryOperations numberAndChoice = do
  it numberAndChoice $ do
    (lhs numberAndChoice) `shouldBe` (rhs numberAndChoice)

lhs :: NumberAndChoice -> [[Rational]]
lhs s = sort' $ fromHallSymbols' (fromJust . fromNumberAndChoice $ s)

rhs :: NumberAndChoice -> [[Rational]]
rhs = sort' . concat . symmetryOperations
