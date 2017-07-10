{-# LANGUAGE Rank2Types #-}
foo :: (forall a . [a] -> [a]) -> ([b],[c]) -> ([b],[c])
foo f (xs,ys)= (f xs, f ys)

bar :: ([a] -> [a]) -> [a] -> [a]
bar f xs = f xs

bar' :: (forall a. [a] -> [a]) -> [a] -> [a]
bar' f xs = f xs

bar'' :: Num b => (forall a. Num a => [a] -> [a]) -> [b] -> [b]
bar'' f xs = f xs
