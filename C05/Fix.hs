halt :: Integral a => a -> [a]
halt 1 = [1]
halt n | even n    = let n' = div n 2 in n':halt n'
       | otherwise = let n' = 3*n+1 in n':halt n'

fix :: (a -> a) -> a
fix f = f (fix f)

factorial :: Int -> Int
factorial = fix (\f n -> if (n==0) then 1 else n * f (n-1))
