data Nature = Zero | Succ Nature deriving (Eq, Ord, Show)

natToInt :: Integral a => Nature -> a
natToInt Zero = 0
natToInt (Succ numNat) = 1 + natToInt numNat

intToNat :: Integral a => a -> Nature
intToNat num
    | num < 0 = error "Number is negative."
    | num == 0 = Zero
    | otherwise = Succ (intToNat (num - 1))

plusNat :: Nature -> Nature -> Nature
plusNat Zero numNat = numNat
plusNat (Succ numF) numB = Succ (plusNat numF numB)

prodNat :: Nature -> Nature -> Nature
prodNat Zero numNat = Zero
prodNat (Succ numF) numB = plusNat (prodNat numF numB) numB

main :: IO()
main = print (natToInt $ prodNat (intToNat 12) (intToNat 10))