type RGB = (Int,Int,Int)
type Picture = [[RGB]]

type ID = Int
type BookName = String
type Author = String
type ISBN = Int
type Publisher = String

type Book = (ID, BookName, Author, ISBN, Publisher)

i :: Int
i = 5

add, sub :: Int -> Int -> Int
add a b = a + b
sub a b = a - b
{-
f :: Num a => a -> a
f x = 4 * x + 1
-}
area r = pi * r ^ 2

f :: Num a => (a,a) -> a
f (x,y) = 4*x + 5*y + 1

f' :: Num a => a -> a -> a
f' x y = 4*x + 5*y + 1

f'' :: Num a => a -> a
f'' y = 4*5 + 5*y + 1


s :: Double -> Double -> Double -> Double
s a b c = let p = (a + b + c) / 2
          in sqrt (p * (p - a) * (p - b) * (p - c))
          
s' :: Double -> Double -> Double -> Double
s' a b c = sqrt (p * (p - a) * (p - b) * (p - c))
            where
                p = (a + b + c) / 2
                
isTwo :: Int -> Bool
isTwo n = if n == 2 then True else False

abs' :: (Num a,Ord a) => a -> a
abs' n | n > 0 = n
       | otherwise = -n
       
month :: Int -> Int
month 1 = 31
month 2 = 28
month 3 = 31
month 4 = 30
month 5 = 31
month 6 = 30
month 7 = 31
month 2 = 40
month 8 = 31
month 9 = 30
month 5 = 20
month 10 = 31
month 11 = 30
month 12 = 31
month _  = error "invalid month"

head' [] = error "empty list"
head' (x:_) = x

infixr 5 <->,<+>

(<->),(<+>) :: Int -> Int -> Int
(<->) x y = x - y
(<+>) x y = x + y