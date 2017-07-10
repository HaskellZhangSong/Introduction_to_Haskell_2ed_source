fact_cps :: (Eq a, Num a) => a -> (a -> t) -> t
fact_cps 0 k = k 1
fact_cps n k = fact_cps (n-1) (\x -> k (n * x))

type Cont a r = (a -> r) -> r

fib_cps :: Int -> (Int -> r) -> r
fib_cps 0 k = k 1
fib_cps 1 k = k 1
fib_cps n k = fib_cps (n-1) (\n1 -> fib_cps (n-2) (\n2 -> k (n1 + n2)))

fib_cps' :: Int -> (Int -> r) -> r
fib_cps' 0 k = k 1
fib_cps' 1 k = k 1
fib_cps' n k = fib_cps (n-2) (\n2 -> fib_cps (n-1) (\n1 -> k (n2 + n1)))
