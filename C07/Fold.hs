-- Fold.hs
(+++) :: [a] -> [a] -> [a]
(+++) = foldr (:)

insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys) | x < y = x : y: ys
                | otherwise = y:insert x ys

isort :: Ord a => [a] -> [a]
isort xs = foldr insert [] xs

skip :: Eq a => a -> [a] -> [a]
skip x [] = [x]
skip x (y:ys) | x == y = (y:ys)
              | otherwise = x:y:ys
              
compress :: Eq a => [a] -> [a]
compress = foldr skip []

snoc :: a -> [a] -> [a]
snoc x = foldr (:) [x]

concat :: [[a]] -> [a]
concat = foldr (++) []

map' :: (a -> b) -> [a] -> [b]
map'  f = foldr (\l ls -> f l : ls) []

reverse' :: [a] -> [a]
reverse' = foldl (flip (:)) []

unwords' :: [String] -> String
unwords' [] =  ""
unwords' ws =  foldr1 (\w s -> w ++ ' ':s) ws

maximum', minimum' :: Ord a => [a] -> a
maximum'  = foldl1 max
minimum'  = foldl1 min