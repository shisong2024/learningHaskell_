import Data.Char (ord, chr, isLower)
-- Judge whether a number is a prime or not.

isPrime :: Integral a => a -> Bool
isPrime 2 = True
isPrime num = num > 1 && all (\n -> num `mod` n /= 0) (takeWhile (\m -> m * m <= num) [3, 5 ..])
-- Function (all condition list) is used to judge whether all elem of list satisfy the condition

-- Use "isPrime" to find the smallest prime greater than fixed integer.

nextPrime :: Integral a => a -> a
nextPrime num
    | odd num = if isPrime num then num else nextPrime(num + 2)
    | otherwise = nextPrime (num + 1)

-- Use Eratosthenes shieves to construct the infinity list of prime. 

primeCon :: Integral a => [a] -> [a]
primeCon (x: list) = x: primeCon [a | a <- list, a `mod` x /= 0]
primes :: [Integer]
primes = primeCon [2..]

-- Construct a function to cipher Caesor Cipher.

shift :: Char -> Int -> Char
shift char num
    | isLower char = chr ((ord char - ord 'a' + num) `mod` 26 + ord 'a')
    | otherwise = char

caesor :: String -> Int -> String
caesor cipher num = map (`shift` num) cipher