import Data.List (genericLength)

avg xs = sum xs / genericLength xs

elem' :: Eq a => a -> [a] -> Bool
elem' a xs = not $ null (filter (==a) xs)

type Weekday = Int
type Year  = Int
type Month = Int
type Day   = Int

week' :: Year -> Day -> Weekday
week' y d = let y1 = y - 1 in
         (y1 + (div y1 4) - (div y1 100) + (div y1 400) + d) `mod` 7
         
isLeapYear :: Int -> Bool
isLeapYear y = (mod y 4 == 0) && (mod y 100 /= 0) || (mod y 400 == 0)

monthDays :: Year -> Month -> Int
monthDays y m | m == 2 = if not $ isLeapYear y then 28 else 29 
              | elem m [1,3,5,7,8,10,12] = 31
              | elem m [4,6,9,11] = 30
              | otherwise = error "invaid month"
              
accDays :: Year -> Month -> Day -> Int
accDays y m d | d > monthDays y m = error "invalid days"
              | otherwise =(sum $ take (m-1) (map (monthDays y) [1..12]))+d
              
week y m d = week' y (accDays y m d)

contains6':: [Int]
contains6' = map (\str->read str::Int) $ filter (elem '6') (map show [1..100])

