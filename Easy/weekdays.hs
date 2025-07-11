isLeapYear :: Int -> Bool
isLeapYear year = (mod year 4 == 0) && (mod year 100 /= 0) || (mod year 400 == 0)

monthDays :: Int -> Int -> Int
monthDays year month
    | month == 2 = if isLeapYear year then 29 else 28
    | elem month [1, 3, 5, 7, 8, 10, 12] = 31
    | elem month [4, 6, 9, 11] = 30
    | otherwise = error "please use correct month."

week :: Int -> Int -> Int
week year day = mod (y + div y 4 - div y 100 + div y 400 + day) 7
    where
        y = year - 1

accDays :: Int -> Int -> Int -> Int
accDays year month day
    | day > monthDays year month || day <= 0 = error "Invalid days."
    | otherwise = sum (take (month - 1) (map (monthDays year) [1..12])) + day

final :: Int -> Int -> Int -> Int
final year month day = week year (accDays year month day)