filter' f xs = [x| x<-xs, f x]

series :: Int -> [Double]
series n = [1 / (2 * (fromIntegral k) + 1) * (-1)^k| k <- [0..n]]
