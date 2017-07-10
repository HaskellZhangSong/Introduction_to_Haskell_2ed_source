module Main where

fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + {-# SCC fib_30 #-} fib (n-2)

main = do
    print $ {-# SCC fib_30 #-} fib 30
    print $ {-# SCC fib_20 #-} fib 20
