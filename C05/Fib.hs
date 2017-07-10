-- Fib.hs
fibonacci :: (Num a, Eq a) => a -> a
fibonacci 0   =  0
fibonacci 1   =  1
fibonacci n   =  fibonacci (n-1) + fibonacci (n-2)

fibs n = map fibonacci [0..n]

fibStep :: Num a => (a, a) -> (a, a)
fibStep (u,v) = (v,u+v)

fibPair :: (Eq a, Num a) => a -> (a, a)
fibPair 0 = (0,1)
fibPair n = fibStep (fibPair (n-1))

fastFib :: (Eq b, Num b) => b -> b
fastFib n = fst (fibPair n)

{-
fibs :: (Enum b, Eq b, Num b) => b -> [b] 
fibs n = map fastFib [1..n]
-}

fibs' n = take n (map fst (iterate fibStep (0,1)))

fib 0 f1 f2 = f2
fib n f1 f2 = fib (n-1) f2 (f1+f2)

fibonacci' n = fib n 1 1

golden :: Fractional a => Int -> [a]
golden n = take n (map (\(x,y) -> x/y) (iterate fibStep (0,1)))

combine :: [(a,a)] -> [(a,a,a)]
combine ((f1,f2):(f3,f4):fs) = (f1,f2,f4):combine ((f3,f4):fs)
combine _ = []

fibPairs :: Int -> [(Int,Int)]
fibPairs n = map fibPair [1..n]

difference :: Int -> [Int]
difference n = map (\(f1,f2,f3)->f1*f3-f2*f2) (combine $ fibPairs n)
