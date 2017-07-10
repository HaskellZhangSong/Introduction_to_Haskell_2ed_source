factors :: Integral a => a -> [a]
factors n = [x| x<- [1..n] , mod n x == 0]

isPrime :: Integral a => a -> Bool
isPrime n = factors n == [1,n]

primes :: Integral a => a -> [a]
primes n = [x| x <- [1..n], isPrime x]

isPrime' :: Integral a => a -> Bool
isPrime' 2 = True
isPrime' p = p > 1 && (all (\n-> p `mod` n /= 0) $ takeWhile (\n->n*n <=p) [3,5..])

nextPrime :: Integer -> Integer
nextPrime a | odd a = if isPrime a then a else nextPrime (a+2)
            | otherwise = nextPrime (a+1)
            
sieve:: (Integral a) => [a] -> [a]
sieve (p:xs) = p: sieve [x| x <- xs, x `mod` p /=0]

primes' = sieve [2..]
