import Prelude hiding (even,odd)

factorial :: Integer -> Integer
factorial n = if n < 0 then error "n is less than 0"
          else if n==0 then 1
          else n * factorial (n-1)
          
mygcd :: Int -> Int -> Int
mygcd x y = if y == 0 then x else mygcd y (mod x y)

power :: Int -> Int -> Int
power 0 0 = error "cannot calculate power 0 0"
power _ 0 = 1
power x n = x * power x (n-1)

power' :: Int -> Int -> Int
power' 0 0 = error "cannot calculate power 0 0"
power' _ 0 = 1
power' x n  | odd n = let p = power' x ((n-1) `div` 2) in x * p * p
            | otherwise = let p = power' x (n `div` 2) in p * p
            
product' [] = 1				     
product' (x:xs) = x * product' xs

last' :: [a] -> a
last'  [] = error "empty list"
last' [x] = x
last' (_:xs) = last' xs

take' n _  | n <= 0 =  []
take' _ []     =  []
take' n (x:xs) =  x : take' (n-1) xs

elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' a (x:xs) = if a == x then True else elem' a xs

total' [] n = n
total' (x:xs) n = total' xs (n+x)

total xs = total' xs 0

even 0 = True
even n = odd (n-1)

odd 0  = False
odd n = even (n-1)