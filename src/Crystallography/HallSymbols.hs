-- | Symmetry Operators Generater of Hall Symbols
--
-- Hallさんの空間群の簡約表記から、対称操作を4x4の行列として生成します
--
-- テストがまだ不十分です。
--
module Crystallography.HallSymbols (
  fromHallSymbols,
  fromHallSymbols',
  hallSymbols,
  ) where

import Data.Maybe
import Data.Char (isSpace)
import Data.List (nub,sort,elemIndex)
import Data.Fixed (mod')
import Data.Ratio
import Data.Matrix hiding (identity,matrix,(<|>))
import qualified Data.Matrix as M (identity)
import Text.ParserCombinators.Parsec

type LatticeSymbol = (Bool,Char)

type NFold = Int

data MatrixSymbol
  = MatrixSymbol {
    minusSign :: Bool, -- -
    nFoldBody :: NFold, -- 1 2 3 4 6
    nFoldSubscript :: Maybe Int, -- 1 2 3 4 5
    nFoldDiagonal :: Maybe Char, -- ' " *
    axisOfRotation :: Maybe Char, -- x y z
    translationVector :: String -- abcnuvwd
  } deriving Show

type OriginShift = (Integer,Integer,Integer)

latticeSymbol :: CharParser () LatticeSymbol
latticeSymbol = do
  sign <- optionMaybe (oneOf "-")
  b <- oneOf "PABCIRF" -- TとSは除外
  return (isJust sign,b)

matrixSymbol :: CharParser () MatrixSymbol
matrixSymbol = do
  sign <- optionMaybe (oneOf "-")
  b <- oneOf "12346"
  s <- optionMaybe (oneOf "12345")
  d <- optionMaybe (oneOf "'*\"")
  a <- optionMaybe (oneOf "xyz")
  t <- vectorSymbol
  if isJust s && isJust d
    then
      fail "Symbols that can not coexist."
    else
      return $ MatrixSymbol (isJust sign) (read [b]) (read . (: []) <$> s) d a t

vectorSymbol :: CharParser () String
vectorSymbol = do
  t <- many (oneOf "abcnuvwd")
  if isValidVectorSymbol t
     then return t
     else fail "unexpected vector symbol."

-- | ベクター記号が順不同になっていないかどうか
isValidVectorSymbol :: String -> Bool
isValidVectorSymbol = f . g
  where
    f a = a == sort a
    g = mapMaybe (`elemIndex` "abcnuvwd")

originShift :: CharParser () OriginShift
originShift = do
  string "("
  va <- integer
  space
  vb <- integer
  space
  vc <- integer
  string ")"
  return (va,vb,vc)

integer :: CharParser () Integer
integer = signed <|> unsigned
  where
    digits = many1 digit
    signed = do
      a <- char '-'
      num <- digits
      return (read $ a : num)
    unsigned = do
      num <- digits
      return (read num)

space' :: CharParser () Char
space' = oneOf " _"

hallSymbols :: CharParser () ( LatticeSymbol, [MatrixSymbol], OriginShift )
hallSymbols = do
  l   <- latticeSymbol
  nat <- many1 (try ms)
  v   <- option (0,0,0) os
  return (l, nat, v)
  where
    ms = do
      space'
      matrixSymbol
    os = do
      space'
      originShift

-- | Generation of equivalent positions
-- HallSymbolsから一般座標を生成します
fromHallSymbols :: String -> Either ParseError [Matrix Rational]
fromHallSymbols s = equivalentPositions
  where
    -- Step 1: Decode space-group symbols
    m = decodeSymbols s
    -- Step 2: Generate symmetry operators
    -- 得られたseiz matrixをgeneratorとして、一般点を生成します
    equivalentPositions = generate <$> m

-- | Generation of equivalent positions (unsafe version)
--
fromHallSymbols' :: String -> [Matrix Rational]
fromHallSymbols' s = case fromHallSymbols s of
  Left e -> error $ show e
  Right mm -> mm

-- | 簡約記号をパーズし、からseiz matrixを復元します
decodeSymbols :: String -> Either ParseError [Matrix Rational]
decodeSymbols s = constructMatrices . restoreDefaultAxis <$> parseHallSymbols s

-- | パーズ済みの簡約記号データからseiz matrixを復元します
constructMatrices :: (LatticeSymbol, [MatrixSymbol], OriginShift) -> [Matrix Rational]
constructMatrices (l,nat,v) = mapOS v $ lattice l ++ map matrix nat

-- | 簡約記号をパーズし、格子と対称芯のL、行列のNATx、Origin ShiftのVに変換します
parseHallSymbols :: String -> Either ParseError (LatticeSymbol, [MatrixSymbol], OriginShift)
parseHallSymbols s = parse hallSymbols ("while reading " ++ show s) s

-- | 簡約記号データの省略された軸情報を復元します
restoreDefaultAxis :: (LatticeSymbol, [MatrixSymbol], OriginShift) -> (LatticeSymbol, [MatrixSymbol], OriginShift)
restoreDefaultAxis ( l, nat, v ) = ( l, mapDA nat, v )

-- | 軸情報復元を配列に適用します
mapDA :: [MatrixSymbol] -> [MatrixSymbol]
mapDA = mapDA' 1 0

-- | 軸情報復元を再帰的に行います
mapDA' :: Int -> NFold -> [MatrixSymbol] -> [MatrixSymbol]
mapDA' _ _ [] = []
mapDA' o p (x:xs) = da o p x : mapDA' (succ o) (nFoldBody x) xs

-- | Default axes
-- 省略された軸情報復元の判定をします
da :: Int -> NFold -> MatrixSymbol -> MatrixSymbol
da 0 _ _ = error "order parameter must be >1."

-- 1. the first rotation has an axis direction of c
-- 1番目の行列記号で軸省略の場合、c軸つまりzとなる
da 1 preceded (MatrixSymbol s n1 n2 n3 Nothing v)
  = MatrixSymbol s n1 n2 n3 (Just 'z') v

-- 2. the second rotation (if N is 2) has an axis direction of
-- 2番目の行列記号が2で軸省略の場合
da 2 preceded (MatrixSymbol s 2 Nothing Nothing Nothing v)
--   a   if preceded by an N of 2 or 4
--   1番目の行列記号が2又は4ならばa軸 -> 2x
  | preceded `elem` [2,4] = MatrixSymbol s 2 Nothing Nothing (Just 'x') v
--   a-b if preceded by an N of 3 or 6
--   1番目の行列記号が3又は6ならばa-b軸 -> 2'z
  | preceded `elem` [3,6] = MatrixSymbol s 2 Nothing (Just '\'') (Just 'z') v

-- 3. the third rotation (N is always 3) has an axis direction of
-- 3番目の行列記号が3の場合、
da 3 _ (MatrixSymbol s 3 Nothing Nothing a v)
--   a+b+c
--   軸はa+b+c -> 3*
  = MatrixSymbol s 3 Nothing (Just '*') a v

-- それ以外の軸省略について記載がみあたらないが、ひとまずc軸を標準としている
da order preceded (MatrixSymbol s n1 n2 n3 Nothing v)
  = MatrixSymbol s n1 n2 n3 (Just 'z') v

-- 軸情報があれば加工せずそのまま
da order preceded (matrixSymbol@(MatrixSymbol _ n1 _ _ _ _))
  = matrixSymbol

-- | 一般点の生成
generate :: [Matrix Rational] -> [Matrix Rational]
generate mm = gn 0 mm mm


-- | 一般点生成の実行部
-- 一般点（equivalent positions）は、その名の通り掛け合わせても、
-- 等価な位置のどこかにしかならない性質があるようで、
-- ジェネレーターとして選ばれた行列を掛け合わせて続けても、
-- この性質をもつ組み合わせであれば、ある一定数以上、要素数が増えない.
-- 繰り返し掛け合わせていき、要素数が増えなくなった段階で得られる行列のセットが一般点である。
-- これを返却することでgeneratorから一般点を生成する計算は完了する。
-- 注意点として、恒等操作が二つあるなどしただけで、計算結果が変化するようである。
-- このため、計算に供する行列はジェネレーターの組み合わせとして正しいかどうか配慮する必要がある。
gn :: Int -> [Matrix Rational] -> [Matrix Rational] -> [Matrix Rational]
gn n s m | length m == length mm = m
         -- 計算が収束しなかった場合、強制終了する. 2018年段階で最大192個に対し、
         -- 仮に全ての要素が∩2だった場合で、2^10=1024個まで可能な限度設定としている
         | n > 10 = error ""
         | otherwise = gn (succ n) s mm
  where
    mm = nub . map (modulus1 . foldl1 multStd) . sequenceA $ [s,m]

-- | 行列の平行移動部分を配列で与えたベクトルで置き換える
setTransform :: Matrix Rational -> [Rational] -> Matrix Rational
setTransform m l = let t = fromList 3 1 l
                       (a,b,c,d) = splitBlocks 3 3 m
                   in joinBlocks (a,t,c,d)

-- | 対称操作の原点を移動する操作を行列のセット全体に適用する
mapOS :: (Integer,Integer,Integer) -> [Matrix Rational] -> [Matrix Rational]
mapOS (va,vb,vc) = map (sn va vb vc)
  where
    sn 0 0 0 m = m
    sn va vb vc m = let v = [ va % 12, vb % 12, vc % 12 ]
                        n = setTransform identity v
                        o = setTransform identity (fmap negate v)
                    in modulus1 $ foldl1 multStd [n,m,o]

-- | 行列の平行移動部分の各要素が [0,1)に収まるようにする
modulus1 :: Real a => Matrix a -> Matrix a
modulus1 m = let (a,b,c,d) = splitBlocks 3 3 m
             in joinBlocks (a,fmap (`mod'` 1) b,c,d)

-- | 行列の回転部分の各要素を符号反転する
ng33 :: Matrix Rational -> Matrix Rational
ng33 m = let (a,b,c,d) = splitBlocks 3 3 m
             in joinBlocks (negate a,b,c,d)

-- | 恒等操作
identity :: Matrix Rational
identity = M.identity 4

-- | 行列セットに、その回転部分の符号反転したものを付与する
mapNg33 :: [Matrix Rational] -> [Matrix Rational]
mapNg33 l = nub [ g m | g <- [ id, ng33 ], m <- l ]

-- | 対称芯
centroSymmetric :: [Matrix Rational] -> [Matrix Rational]
centroSymmetric = mapNg33

-- | 副格子に基づいた操作
set :: [[Rational]] -> [Matrix Rational]
set = map (setTransform identity)

-- | Table 1. Lattice Symbol T
-- 副格子の行列を生成する
lattice :: LatticeSymbol -> [Matrix Rational]
lattice (False, l) =                  tbl1 l
lattice ( True, l) = centroSymmetric (tbl1 l)

tbl1 :: Char -> [Matrix Rational]
tbl1 'P' = set [[ 0, 0, 0 ]]
tbl1 'A' = set [[ 0, 0, 0 ], [   0, 1%2, 1%2 ]]
tbl1 'B' = set [[ 0, 0, 0 ], [ 1%2,   0, 1%2 ]]
tbl1 'C' = set [[ 0, 0, 0 ], [ 1%2, 1%2,   0 ]]
tbl1 'I' = set [[ 0, 0, 0 ], [ 1%2, 1%2, 1%2 ]]
tbl1 'R' = set [[ 0, 0, 0 ], [ 1%3, 2%3, 2%3 ], [ 2%3, 1%3, 1%3 ]]
tbl1 'F' = set [[ 0, 0, 0 ], [   0, 1%2, 1%2 ], [ 1%2,   0, 1%2 ], [ 1%2, 1%2,  0 ]]
tbl1  c  = error $ show c

-- | translation vector
-- 複数のT記号を合算したベクトル
tv :: String -> [Rational]
tv ch | null ch        = [0,0,0]
      | otherwise      = fmap (`mod'` 1) $ foldl1 (zipWith (+)) $ map tbl2 ch

-- | Table 2. Translation symbol T
tbl2 :: Char -> [Rational]
tbl2 'a' = [ 1%2,   0,   0 ]
tbl2 'b' = [   0, 1%2,   0 ]
tbl2 'c' = [   0,   0, 1%2 ]
tbl2 'n' = [ 1%2, 1%2, 1%2 ]
tbl2 'u' = [ 1%4,   0,   0 ]
tbl2 'v' = [   0, 1%4,   0 ]
tbl2 'w' = [   0,   0, 1%4 ]
tbl2 'd' = [ 1%4, 1%4, 1%4 ]
tbl2  c  = error $ show c

-- | seiz matrix
-- generaterとなるmatrix
matrix :: MatrixSymbol -> Matrix Rational
matrix (MatrixSymbol False 3 (Just 1) Nothing (Just 'z') _) = setTransform matrix3z [0,0,1%3]
matrix (MatrixSymbol False 3 (Just 2) Nothing (Just 'z') _) = setTransform matrix3z [0,0,2%3]
matrix (MatrixSymbol False 4 (Just 1) Nothing (Just 'z') _) = setTransform matrix4z [0,0,1%4]
matrix (MatrixSymbol False 4 (Just 3) Nothing (Just 'z') _) = setTransform matrix4z [0,0,3%4]
matrix (MatrixSymbol False 6 (Just 1) Nothing (Just 'z') _) = setTransform matrix6z [0,0,1%6]
matrix (MatrixSymbol False 6 (Just 2) Nothing (Just 'z') _) = setTransform matrix6z [0,0,2%6]
matrix (MatrixSymbol False 6 (Just 4) Nothing (Just 'z') _) = setTransform matrix6z [0,0,4%6]
matrix (MatrixSymbol False 6 (Just 5) Nothing (Just 'z') _) = setTransform matrix6z [0,0,5%6]
-- table (MatrixSymbol s n1 n2 n3 a t) = replaceTransform (table'' s n1 n3 a) (vector t)
matrix (MatrixSymbol s n1 n2 n3 a t) = setTransform (ng33if s $ tbl345 n1 n3 a) (tv t)

ng33if flag = if flag then ng33 else id

matrix3z = tbl345 3 Nothing (Just 'z')
matrix4z = tbl345 4 Nothing (Just 'z')
matrix6z = tbl345 6 Nothing (Just 'z')

-- | Rotation Table
-- Table 3. Rotation symbol N Aforprincipal axes
-- Table 4. Rotation symbol N a for face-diagonal axes
-- Table 5. Rotation matrix for the body-diagonal axis
tbl345 :: Int -> Maybe Char -> Maybe Char -> Matrix Rational
-- table 3
tbl345 1  Nothing     _         = fromLists [[ 1, 0, 0, 0], [ 0, 1, 0, 0], [ 0, 0, 1, 0], [ 0, 0, 0, 1]]
-- Symbol Nx
-- Rotation axis a
tbl345 2  Nothing    (Just 'x') = fromLists [[ 1, 0, 0, 0], [ 0,-1, 0, 0], [ 0, 0,-1, 0], [ 0, 0, 0, 1]]
tbl345 3  Nothing    (Just 'x') = fromLists [[ 1, 0, 0, 0], [ 0, 0,-1, 0], [ 0, 1,-1, 0], [ 0, 0, 0, 1]]
tbl345 4  Nothing    (Just 'x') = fromLists [[ 1, 0, 0, 0], [ 0, 0,-1, 0], [ 0, 1, 0, 0], [ 0, 0, 0, 1]]
tbl345 6  Nothing    (Just 'x') = fromLists [[ 1, 0, 0, 0], [ 0, 1,-1, 0], [ 0, 1, 0, 0], [ 0, 0, 0, 1]]
-- Symbol Ny
-- Rotation axis b
tbl345 2  Nothing    (Just 'y') = fromLists [[-1, 0, 0, 0], [ 0, 1, 0, 0], [ 0, 0,-1, 0], [ 0, 0, 0, 1]]
tbl345 3  Nothing    (Just 'y') = fromLists [[-1, 0, 1, 0], [ 0, 1, 0, 0], [-1, 0, 0, 0], [ 0, 0, 0, 1]]
tbl345 4  Nothing    (Just 'y') = fromLists [[ 0, 0, 1, 0], [ 0, 1, 0, 0], [-1, 0, 0, 0], [ 0, 0, 0, 1]]
tbl345 6  Nothing    (Just 'y') = fromLists [[ 0, 0, 1, 0], [ 0, 1, 0, 0], [-1, 0, 1, 0], [ 0, 0, 0, 1]]
-- Symbol Nz
-- Rotation axis c
tbl345 2  Nothing    (Just 'z') = fromLists [[-1, 0, 0, 0], [ 0,-1, 0, 0], [ 0, 0, 1, 0], [ 0, 0, 0, 1]]
tbl345 3  Nothing    (Just 'z') = fromLists [[ 0,-1, 0, 0], [ 1,-1, 0, 0], [ 0, 0, 1, 0], [ 0, 0, 0, 1]]
tbl345 4  Nothing    (Just 'z') = fromLists [[ 0,-1, 0, 0], [ 1, 0, 0, 0], [ 0, 0, 1, 0], [ 0, 0, 0, 1]]
tbl345 6  Nothing    (Just 'z') = fromLists [[ 1,-1, 0, 0], [ 1, 0, 0, 0], [ 0, 0, 1, 0], [ 0, 0, 0, 1]]
-- table 4
-- preceded Nx
--   b-c
tbl345 2 (Just '\'') (Just 'x') = fromLists [[-1, 0, 0, 0], [ 0, 0,-1, 0], [ 0,-1, 0, 0], [ 0, 0, 0, 1]]
--   b+c
tbl345 2 (Just '"')  (Just 'x') = fromLists [[-1, 0, 0, 0], [ 0, 0, 1, 0], [ 0, 1, 0, 0], [ 0, 0, 0, 1]]
-- preceded Ny
--   a-c
tbl345 2 (Just '\'') (Just 'y') = fromLists [[ 0, 0,-1, 0], [ 0,-1, 0, 0], [-1, 0, 0, 0], [ 0, 0, 0, 1]]
--   a+c
tbl345 2 (Just '"')  (Just 'y') = fromLists [[ 0, 0, 1, 0], [ 0,-1, 0, 0], [ 1, 0, 0, 0], [ 0, 0, 0, 1]]
-- preceded Nz
--   a-b
tbl345 2 (Just '\'') (Just 'z') = fromLists [[ 0,-1, 0, 0], [-1, 0, 0, 0], [ 0, 0,-1, 0], [ 0, 0, 0, 1]]
--   a+b
tbl345 2 (Just '"')  (Just 'z') = fromLists [[ 0, 1, 0, 0], [ 1, 0, 0, 0], [ 0, 0,-1, 0], [ 0, 0, 0, 1]]
-- table 5
--   a+b+c
tbl345 3 (Just '*')   _         = fromLists [[ 0, 0, 1, 0], [ 1, 0, 0, 0], [ 0, 1, 0, 0], [ 0, 0, 0, 1]]
-- error
tbl345 a b            c         = error $ show (a,b,c)

{--


[refereces]

1. Space-Group Notation with an Explicit Origin
   [URL] S.R. Hall; Space-Group Notation with an Explicit Origin ; Acta Cryst. (1981). A37, 517-525
2. Concise Space-Group Symbols
   [URL] http://cci.lbl.gov/sginfo/hall_symbols.html#Table_3



--}
