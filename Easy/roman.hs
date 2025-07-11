-- Our target is to design a function to convert number to Rome number.
romeNotation :: [String]
romeNotation = ["M", "CM", "D", "CD", "C", "XC", "L", "XL", "X", "IX", "V", "IV", "I"]

corrNum :: [Int]
corrNum = [1000, 900, 500, 400, 100, 90, 50, 40, 10, 9, 5, 4, 1]

pair :: [(Int, String)]
pair = zip corrNum romeNotation

findLess :: Int -> (Int, String)
findLess num = head (dropWhile (\(a, _) -> a > num) pair)

final :: Int -> String
final 0 = ""
final num = snd tup ++ final (num - fst tup)
    where tup = findLess num