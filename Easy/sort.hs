-- Now I'll try to rewrite all the sort function
-- FIRST: bubble sort

bubble :: Ord a => [a] -> [a]
bubble [] = []
bubble [x] = [x]
bubble (x: y: list)
    | x <= y = x: bubble (y: list)
    | otherwise = y: bubble (x: list)

bSort :: Ord a => [a] -> [a]
bSort [] = []
bSort list = bSort (init after) ++ [last after]
    where after = bubble list

-- SECOND: insert sort
insert :: Ord a => a -> [a] -> [a]
insert num [] = [num]
insert num (fir: list)
    | num <= fir = num: fir: list
    | otherwise = fir: insert num list

iSort :: Ord a => [a] -> [a]
iSort = foldr insert []

--THIRD: selection sort
delete :: Ord a => a -> [a] -> [a]
delete num (x: list)
    | num == x = list
    | otherwise = x: delete num list

sSort :: Ord a => [a] -> [a]
sSort [] = []
sSort list = mini: sSort (delete mini list)
    where mini = minimum list

--FOURTH: quick sort
qSort :: Ord a => [a] -> [a]
qSort [] = []
qSort (x: list) = qSort front ++ [x] ++ qSort behind
    where front  = filter (< x) list
          behind = filter (> x) list

--FIFTH: merge sort
--I think there's no bugs... But stack overflow...
merge :: Ord a => [a] -> [a] -> [a]
merge x [] = x
merge [] y = y
merge (x: listF) (y: listB)
    | x < y = x: merge listF (y: listB)
    | otherwise = y: merge (x: listF) listB

mSort :: Ord a => [a] -> [a]
mSort [] = []
mSort [x] = [x]
mSort list = merge (mSort listF) (mSort listB)
    where len = length list `div` 2
          (listF, listB) = splitAt len list

{-
mSort [2, 1, 3, 1, 4, 1, 5, 6, 7]
merge (mSort [2, 1, 3, 1]) (mSort [4, 1, 5, 6, 7])
merge (merge (mSort [2, 1]) (mSort [3, 1])) (merge (mSort [4, 1]) (mSort [5, 6, 7]))
merge (merge [1, 2] [1, 3]) (merge [1, 4] merge (mSort [5]) (mSort [6, 7]))
-}