merge :: Ord a => [a] -> [a] -> [a]
merge [] x = x
merge y [] = y
merge (x: listF) (y: listB)
    | x < y = x: merge listF (y: listB)
    | x > y = y: merge (x: listF) listB
    | otherwise = x: merge listF listB

ham :: [Integer]
ham = 1: merge (map (*2) ham) (merge (map (*3) ham) (map (*5) ham))

main :: IO()
main = print (take 15 ham)