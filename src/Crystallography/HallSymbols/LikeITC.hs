module Crystallography.HallSymbols.LikeITC (
--  generatorSelected,
  generatorsSelected,
  symmetryOperations,
  ) where

import Data.Maybe
import Data.List (nub)
import Data.Matrix

import Data.Matrix.SymmetryOperationsSymbols

import Crystallography.HallSymbols
import Crystallography.HallSymbols.SpacegroupSymbols

import Data.Matrix.AsXYZ
import Data.Matrix.SymmetryOperationsSymbols

generateLikeITC selected = nub $ gen [] selected
  where
    gen a  []    = a
    gen [] (b:c) = gen [b] c
    gen a  (b:c) = gen (a ++ multSymop b a) c

hogehoge :: [[Matrix Rational]] -> IO ()
hogehoge mm = mapM_ putStrLn hoge
  where
    hoge :: [String]
    hoge = concat $ map fromMatrix' h
    h :: [Matrix Rational]
    h = concat mm

generatorsSelected numberAndChoice = case select <$> symop <*> indices of
  Just a -> a
  Nothing -> []
  where
    symbol = fromNumberAndChoice numberAndChoice
    symop = fromHallSymbols' <$> symbol
    indices = selectGeneratorIndices numberAndChoice

generatorsSelected' numberAndChoice = case select' <$> symop <*> indices of
  Just a -> a
  Nothing -> []
  where
    symbol = fromNumberAndChoice numberAndChoice
    symop = fromHallSymbols' <$> symbol
    indices = selectGeneratorIndices numberAndChoice

symmetryOperations numberAndChoice = map generateLikeITC $ generatorsSelected' numberAndChoice

selectGeneratorIndices numberAndChoice = lookup numberAndChoice generatorSelected'

select symops (a,b) = [[ symops !! n | n <- idx] | idx <- indices]
  where
    indices = [a,b]

select' symops (a,b) = [[ symops !! n | n <- idx] | idx <- indices]
  where
    indices = [ [c] ++ b | c <- a]

generatorSelected' :: [([Char], ([Int], [Int]))]
generatorSelected' = [

  ("1", ([0],[])),

  ("2", ([0],[1])),

  ("3:b", ([0],[1])),
  ("3:c", ([0],[1])),

  ("4:b",([0],[1])),
  ("4:c",([0],[1])),

  ("5:b1",([0,1],[2])),
  ("5:b2",([0,1],[2])),
  ("5:b3",([0,1],[2])),

  ("5:c1",([0,1],[2])),
  ("5:c2",([0,1],[2])),
  ("5:c3",([0,1],[2])),

  ("6:b",([0],[1])),
  ("6:c",([0],[1])),

  ("7:b1",([0],[1])),
  ("7:b2",([0],[1])),
  ("7:b3",([0],[1])),
  ("7:c1",([0],[1])),
  ("7:c2",([0],[1])),
  ("7:c3",([0],[1])),

  ("8:b1",([0,1],[2])),
  ("8:b2",([0,1],[2])),
  ("8:b3",([0,1],[2])),
  ("8:c1",([0,1],[2])),
  ("8:c2",([0,1],[2])),
  ("8:c3",([0,1],[2])),

  ("9:b1",([0,1],[2])),
  ("9:b2",([0,1],[2])),
  ("9:b3",([0,1],[2])),
  ("9:c1",([0,1],[2])),
  ("9:c2",([0,1],[2])),
  ("9:c3",([0,1],[2])),

  ("10:b",([0],[2,1])),
  ("10:c",([0],[2,1])),

  ("11:b",([0],[2,1])),
  ("11:c",([0],[2,1])),

-- TODO:CHECK
  ("12:b1",([0,1],[4,2])),
  ("12:b2",([0,1],[4,2])),
  ("12:b3",([0,1],[4,2])),
  ("12:c1",([0,1],[4,2])),
  ("12:c2",([0,1],[4,2])),
  ("12:c3",([0,1],[4,2])),

-- TODO:CHECK
  ("13:b1",([0],[2,1])),
  ("13:b2",([0],[2,1])), 
  ("13:b3",([0],[2,1])),
  ("13:c1",([0],[2,1])),
  ("13:c2",([0],[2,1])),
  ("13:c3",([0],[2,1])),

-- TODO:CHECK
  ("14:b1",([0],[2,1])),
  ("14:b2",([0],[2,1])),
  ("14:b3",([0],[2,1])),
  ("14:c1",([0],[2,1])),
  ("14:c2",([0],[2,1])),
  ("14:c3",([0],[2,1])),



  ("141:1", ([0,1],[8,2,19,4])),
  ("141:2", ([0,1],[13,5,26,4]))
  ]

