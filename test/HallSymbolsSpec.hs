module HallSymbolsSpec where

import Test.Hspec

import Control.Exception (evaluate)
import Crystallography.HallSymbols
import Crystallography.HallSymbols.SpacegroupSymbols (NumberAndChoice,HallName,spacegroupSymbols)
import Data.List (sort,nub)
import Data.Matrix (Matrix,toList,zero,identity)
import Data.Matrix.AsXYZ (fromXYZ)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)

-- for check about equivalent
sort' :: Ord a => [Matrix a] -> [[a]]
sort' xs = sort . map toList $ xs

spec :: Spec
spec = do


  describe "sort'" $ do

    it "empty list is not equivalent with a 0 matrix." $ do
      sort' [] `shouldNotBe` (sort' [zero 4 4])

    it "a 1 matrix is not equivalent with a 0 matrix." $ do
      sort' [identity 4] `shouldNotBe` (sort' [zero 4 4])

    it "0 and 1 matrices is not equivalent with a 0 matrix." $ do
      sort' [zero 4 4,identity 4] `shouldNotBe` (sort' [zero 4 4])

    it "0 and 1 matrices is equivalent with 1 and 0 matrix." $ do
      sort' [zero 4 4,identity 4] `shouldBe` (sort' [identity 4,zero 4 4])

  describe "Crystallography.HallSymbols.hallSymbols" $ do
    return ()

  describe "Crystallography.HallSymbols.fromHallSymbols" $ do

    it "[jpn] P 1 wo parse suru to identity ni naru" $ do
      fromHallSymbols "P 1" `shouldBe` (Right [identity 4])

    it "[jpn] P_1 wo parse suru to identity ni naru" $ do
      fromHallSymbols "P_1" `shouldBe` (Right [identity 4])

    it "[jpn] 'C -2yc' wo parse suru to 'C 1 c 1' ni naru" $ do -- 9:b1
      sort' <$> fromHallSymbols "C -2yc"
        `shouldBe`
         (Right . sort' . map fromXYZ $
         ["x,y,z", "x,-y,z+1/2", "x+1/2,y+1/2,z", "x+1/2,-y+1/2,z+1/2"])

    it "[jpn] 'I 4bw 2aw -1bw' wo parse suru to 'I 41/a c d ORIGIN CHOICE 1' ni naru" $ do -- 142:1
      sort' <$> fromHallSymbols "I 4bw 2aw -1bw"
        `shouldBe` (Right . sort' . map fromXYZ $
        ["x,y,z","-x+1/2,-y+1/2,z+1/2","-y,x+1/2,z+1/4","y+1/2,-x,z+3/4",
        "-x+1/2,y,-z+1/4","x,-y+1/2,-z+3/4","y+1/2,x+1/2,-z","-y,-x,-z+1/2",
        "-x,-y+1/2,-z+1/4","x+1/2,y,-z+3/4","y,-x,-z","-y+1/2,x+1/2,-z+1/2",
        "x+1/2,-y+1/2,z","-x,y,z+1/2","-y+1/2,-x,z+1/4","y,x+1/2,z+3/4",
        "x+1/2,y+1/2,z+1/2","-x,-y,z","-y+1/2,x,z+3/4","y,-x+1/2,z+1/4",
        "-x,y+1/2,-z+3/4","x+1/2,-y,-z+1/4","y,x,-z+1/2","-y+1/2,-x+1/2,-z",
        "-x+1/2,-y,-z+3/4","x,y+1/2,-z+1/4","y+1/2,-x+1/2,-z+1/2","-y,x,-z",
        "x,-y,z+1/2","-x+1/2,y+1/2,z","-y,-x+1/2,z+3/4","y+1/2,x,z+1/4"])

    it "[jpn] 'P 61 2 (0 0 -1)' wo parse suru to 'P 61 2 2' ni naru" $ do -- 178
      sort' <$> fromHallSymbols "P 61 2 (0 0 -1)"
      `shouldBe` (Right . sort' . map fromXYZ $
      ["x,y,z","-y,x-y,z+1/3","y-x,-x,z+2/3","-x,-y,z+1/2",
      "y,y-x,z+5/6","x-y,x,z+1/6","y,x,-z+1/3","x-y,-y,-z",
      "-x,y-x,-z+2/3","-y,-x,-z+5/6","y-x,y,-z+1/2","x,x-y,-z+1/6"])

    it "[jpn] '-I 4bd 2c 3' wo parse suru to 'I a -3 d' ni naru" $ do -- 230
      sort' <$> fromHallSymbols "-I 4bd 2c 3"
      `shouldBe` (Right . sort' . map fromXYZ $
      ["x,y,z","-x+1/2,-y,z+1/2","-x,y+1/2,-z+1/2","x+1/2,-y+1/2,-z",
      "z,x,y","z+1/2,-x+1/2,-y","-z+1/2,-x,y+1/2","-z,x+1/2,-y+1/2",
      "y,z,x","-y,z+1/2,-x+1/2","y+1/2,-z+1/2,-x","-y+1/2,-z,x+1/2",
      "y+3/4,x+1/4,-z+1/4","-y+3/4,-x+3/4,-z+3/4","y+1/4,-x+1/4,z+3/4","-y+1/4,x+3/4,z+1/4",
      "x+3/4,z+1/4,-y+1/4","-x+1/4,z+3/4,y+1/4","-x+3/4,-z+3/4,-y+3/4","x+1/4,-z+1/4,y+3/4",
      "z+3/4,y+1/4,-x+1/4","z+1/4,-y+1/4,x+3/4","-z+1/4,y+3/4,x+1/4","-z+3/4,-y+3/4,-x+3/4",
      "-x,-y,-z","x+1/2,y,-z+1/2","x,-y+1/2,z+1/2","-x+1/2,y+1/2,z",
      "-z,-x,-y","-z+1/2,x+1/2,y","z+1/2,x,-y+1/2","z,-x+1/2,y+1/2",
      "-y,-z,-x","y,-z+1/2,x+1/2","-y+1/2,z+1/2,x","y+1/2,z,-x+1/2",
      "-y+1/4,-x+3/4,z+3/4","y+1/4,x+1/4,z+1/4","-y+3/4,x+3/4,-z+1/4","y+3/4,-x+1/4,-z+3/4",
      "-x+1/4,-z+3/4,y+3/4","x+3/4,-z+1/4,-y+3/4","x+1/4,z+1/4,y+1/4","-x+3/4,z+3/4,-y+1/4",
      "-z+1/4,-y+3/4,x+3/4","-z+3/4,y+3/4,-x+1/4","z+3/4,-y+1/4,-x+3/4","z+1/4,y+1/4,x+1/4",
      "x+1/2,y+1/2,z+1/2","-x,-y+1/2,z","-x+1/2,y,-z","x,-y,-z+1/2",
      "z+1/2,x+1/2,y+1/2","z,-x,-y+1/2","-z,-x+1/2,y","-z+1/2,x,-y",
      "y+1/2,z+1/2,x+1/2","-y+1/2,z,-x","y,-z,-x+1/2","-y,-z+1/2,x",
      "y+1/4,x+3/4,-z+3/4","-y+1/4,-x+1/4,-z+1/4","y+3/4,-x+3/4,z+1/4","-y+3/4,x+1/4,z+3/4",
      "x+1/4,z+3/4,-y+3/4","-x+3/4,z+1/4,y+3/4","-x+1/4,-z+1/4,-y+1/4","x+3/4,-z+3/4,y+1/4",
      "z+1/4,y+3/4,-x+3/4","z+3/4,-y+3/4,x+1/4","-z+3/4,y+1/4,x+3/4","-z+1/4,-y+1/4,-x+1/4",
      "-x+1/2,-y+1/2,-z+1/2","x,y+1/2,-z","x+1/2,-y,z","-x,y,z+1/2",
      "-z+1/2,-x+1/2,-y+1/2","-z,x,y+1/2","z,x+1/2,-y","z+1/2,-x,y",
      "-y+1/2,-z+1/2,-x+1/2","y+1/2,-z,x","-y,z,x+1/2","y,z+1/2,-x",
      "-y+3/4,-x+1/4,z+1/4","y+3/4,x+3/4,z+3/4","-y+1/4,x+1/4,-z+3/4","y+1/4,-x+3/4,-z+1/4",
      "-x+3/4,-z+1/4,y+1/4","x+1/4,-z+3/4,-y+1/4","x+3/4,z+3/4,y+3/4","-x+1/4,z+1/4,-y+3/4",
      "-z+3/4,-y+1/4,x+1/4","-z+1/4,y+1/4,-x+3/4","z+1/4,-y+3/4,-x+1/4","z+3/4,y+3/4,x+3/4"])

  describe "Crystallography.HallSymbols.fromHallSymbols'" $ do

    it "throws an exception if used with an empty string" $ do
      evaluate (fromHallSymbols' "") `shouldThrow` anyException

    it "throws an exception if used with an wrong string" $ do
      evaluate (fromHallSymbols' "PP") `shouldThrow` anyException

  describe "unique" $ do
    testUniqueAll' 527
    mapM_ testUnique $ filter (/= 68) [1..230]
    mapM_ (uncurry testUnique') $ [(68,9)]

testUniqueAll = testUniqueAll' (length allNumberAndChoice)

testUniqueAll' c = do
     it ("unique count ==" ++ show c) $ do
       (length . uniqueSpacegroups $ allNumberAndChoice) `shouldBe` c

testUnique n = do
     it ("no." ++ (show n) ++ " uniques = " ++ (show $ length numberAndChoices)) $ do
       (length . uniqueSpacegroups $ numberAndChoices) `shouldBe` (length numberAndChoices)
  where
    numberAndChoices = filter' n $ allNumberAndChoice

testUnique' n c = do
     it ("no." ++ (show n) ++ " uniques = " ++ (show c) ++ " (total: " ++ (show $ length numberAndChoices) ++ ")") $ do
       (length . uniqueSpacegroups $ numberAndChoices) `shouldBe` c
  where
    numberAndChoices = filter' n $ allNumberAndChoice

fromNumberAndChoice :: NumberAndChoice -> Maybe HallName
fromNumberAndChoice numberAndChoice = lookup numberAndChoice hallNames
  where
    hallNames = map (\(a,b,c) -> (a,c)) spacegroupSymbols

lhs :: NumberAndChoice -> [[Rational]]
lhs s = sort' $ fromHallSymbols' (fromJust . fromNumberAndChoice $ s)

allNumberAndChoice :: [String]
allNumberAndChoice = map (\(a,b,d) -> a) spacegroupSymbols

uniqueSpacegroups :: [NumberAndChoice] -> [[[Rational]]]
uniqueSpacegroups mm = nub $ map lhs mm

filter' :: Int -> [String] -> [String]
filter' i = filter (\a -> (head . splitOn ":" $ a) == s)
  where
    s = show i


