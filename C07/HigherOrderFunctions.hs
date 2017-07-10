power2 :: Num a => [a] -> [a]
power2 [] = []
power2 (x:xs) = x^2 : power2 xs

plus1 :: Num a => [a] -> [a]
plus1 [] = []
plus1 (x:xs) = (x+1) : plus1 xs

fix1 :: (a -> a) -> a
fix1 f = f (fix1 f)

fix2 :: Eq a => (a -> a) -> a -> a
fix2 f x | x == f x  = x
         | otherwise = fix2 f (f x)

fix3 :: (t -> t -> Bool) -> (t -> t) -> t -> t
fix3 c f x | c x  (f x)  = x
           | otherwise = fix3 c f (f x)
           
apply :: (a -> a) -> Int -> a -> a
apply f 0 x = x
apply f n x = apply f (n-1) (f x)