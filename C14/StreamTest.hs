import Control.Monad.Stream
import Control.Monad

nats = return 0 `mplus` (nats >>= (return . (+1)))

natpair :: Stream (Int,Int)
natpair= do
    i <- nats
    j <- nats
    return (i,j)

pythagorean_triples :: Stream (Int,Int,Int)
pythagorean_triples = do
     i <- nats
     guard $ i > 0
     j <- nats
     guard $ j > 0
     k <- nats
     guard $ k > 0
     guard $ i*i + j*j == k*k
     return (i,j,k)

