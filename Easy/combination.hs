import Data.List (delete, tails, transpose, minimumBy)
import Data.Ord (comparing)

-- define a function to enum all the permutations from 1 to n. 

-- I'll define a function to insert a num in a list of all positions.
insert :: Integral a => a -> [a] -> [[a]]
insert num [] = [[num]]
insert num (x: list) = (num: x: list): [x: tmp | tmp <- insert num list]

permus :: Integral a => a -> [[a]]
permus 0 = [[]]
permus num
    | num <= 0 = error "Please input a positive num."
    | otherwise = concat [insert num list | list <- permus (num - 1)]

-- define a function to solve derangement problem

derangeTmp :: [Int] -> [[Int]]
derangeTmp [] = [[]]
derangeTmp list = [x: listB | x <- list, x /= length list, listB <- derangeTmp (delete x list)]

derange :: Int -> [[Int]]
derange num = map reverse (derangeTmp [1..num])

-- define a function to list all the subset of a fixed set. 

subset :: [a] -> [[a]]
subset [] = [[]]
subset (x: list) = subset list ++ [x: set | set <- subset list]
-- If there's need using this function, select function subsequences from Data.List to replace.

-- Define a function to select n elems from a set. Both elem-repeatable version and not are required.

selNo :: Int -> [a] -> [[a]]
selNo 0 _ = [[]]
selNo eleNum list = [x: listM | x: listN <- tails list, listM <- selNo (eleNum - 1) listN]

selYes :: Int -> [a] -> [[a]]
selYes 0 _ = [[]]
selYes eleNum list = [x: listM | x <- list, listM <- selYes (eleNum - 1) list]

-- Define a function to solve "Eight Queen" Problem. Hint: use function permus.

-- This function is used to assure that there're no queens on the same diagnal.
noSameDiag :: [Int] -> Bool
noSameDiag [] = True
noSameDiag (x: list) = and [abs (fir - i) /= abs (sec - p) | (i, p) <- zipper] && noSameDiag list
    where (fir, sec): zipper = zip [1..] (x: list)

queens :: Int -> [[Int]]
queens rowNum = filter noSameDiag (permus rowNum)

-- Define a function to compute matrices product and use it to calculate Fibonacci.

mProd :: Num a => [[a]] -> [[a]] -> [[a]]
mProd firMat secMat
    | length (transpose firMat) /= length secMat = error "Invalid input."
    | otherwise = [[sum (zipWith (*) firRow secCol) | secCol <- transpose secMat] | firRow <- firMat]

basic :: Integral a => [[a]]
basic = [[1, 1], [1, 0]]

fibMat :: Integral a => a -> [[a]]
fibMat 1 = basic
fibMat num
    | even num = let cnt = num `div` 2 in mProd (fibMat cnt) (fibMat cnt)
    | otherwise = let cnt = (num - 1) `div` 2 in mProd (mProd (fibMat cnt) (fibMat cnt)) basic

{- 
Now give you a matrix of the distance from a place to another.
You need to define a function to give the shortest way from a place to another in fixed steps.
-}

{-
Example distance matrix:
  A  , B, C  , D
[[0  , 2, inf, 3  ],
 [2  , 0, 3  , 4  ],
 [inf, 3, 0  , inf],
 [3  , 4, inf, 0  ]]
 Our target is to find a shortest way from and to anywhere with fixed step.
-}

-- FIRST: deal the matrix.
type Weight = [[(Double, String)]]

dealMat :: [[Double]] -> [String] -> Weight
dealMat distMat nameL = [zip rowD rowN | (rowD, rowN) <- zip distMat nameAL]
    where nameAL = [[strF ++ "->" ++ strB | strB <- nameL] | strF <- nameL]

-- SECOND: construct the kernel function. 
{-
This function is used to plus tuple in matrix already having been delt.
Notice: the first argument must have only one "->".
-}
plusT :: Num a => (a, String) -> (a, String) -> (a, String)
plusT (numF, strF) (numB, strB) = (numF + numB, str)
    where str = strF ++ dropWhile (/= '-') strB

{-
The fundamenal of this problem is 
shortestDist of n = minimum of (dist of 1 + shortestDist of (n - 1))
-}
-- This function can execute one of the step.
step :: Weight -> Weight -> Weight
step wtF wtB = [[minimumBy (comparing fst) (zipWith plusT rowF rowB) | rowB <- transpose wtB] | rowF <- wtF]

iter :: Int -> (a -> a) -> a -> a
iter 0 _ val = val
iter cnt func val = func (iter (cnt - 1) func val)

steps :: Int -> Weight -> Weight
steps cnt weight = iter cnt (step weight) weight

-- THIRD: calculate the final answer. 
-- Via following function to find the fix point of function steps then return the answer. 
fixN :: (Weight -> Weight) -> Weight -> Weight
fixN func val
    | front == behind = val
    | otherwise = fixN func (func val)
    where front  = [map fst listF | listF <- val]
          behind = [map fst listB | listB <- func val]

final :: [[Double]] -> [String] -> Weight
final distMat nameL = let mid = dealMat distMat nameL in fixN (step mid) mid

-- FORTH: test.
inf :: Fractional a => a
inf = 1 / 0

graph :: [[Double]]
graph = [[0  , 2, inf, 7  ],
 [2  , 0, 3  , 5  ],
 [inf, 3, 0  , inf],
 [7  , 5, inf, 0  ]]

main :: IO()
main = print (final graph ["A", "B", "C", "D"])