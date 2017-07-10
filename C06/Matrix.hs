import Data.List (transpose)

infixl  5  |*|

(|*|) :: Num a => [[a]] -> [[a]] -> [[a]]
(|*|) a b = [[ sum $ zipWith (*) ar bc | bc <- transpose b ] | ar <- a]

unit = [[1,1],[1,0]]

fib 1 = unit
fib n | even n = let m = fib (div n 2) in m |*| m
      | otherwise = let m = fib (div (n-1) 2) in m |*| unit |*| m