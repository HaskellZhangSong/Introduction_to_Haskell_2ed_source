-- ContMonad1.hs
{-# LANGUAGE DeriveFunctor #-}

newtype Cont r a = Cont {runCont :: (a -> r) -> r}
                 deriving Functor

instance Applicative (Cont r) where
    pure a = Cont $ \k -> k a
    -- cab        :: Cont r (a -> b) = ((a -> b) -> r) -> r
    -- ca         :: Cont r a = (a -> r) -> r
    -- cab <*> ca :: Cont r b = (b -> r) -> r
    cab <*> ca = Cont $ \br -> runCont cab (\ab -> runCont ca (\a -> br (ab a)))

instance Monad (Cont r) where
    return = pure
    -- ca         :: Cont r a = (a -> r) -> r
    -- acb        :: a -> Cont r b = a -> ((b -> r) -> r))
    -- ca >>= acb :: Cont r b = (b -> r) -> r
    ca >>= acb = Cont $ \br -> runCont ca (\a -> runCont (acb a) (\b -> br b))


fact_cps :: Int -> Cont r Int
fact_cps 0 = return 1
fact_cps n = do
         n1 <- fact_cps (n - 1)
         return (n * n1)

plus_1 :: Int -> Cont r Int
plus_1 n = return (n + 1)


div_10 :: Int -> Cont r Int
div_10 n = return (div n 10)

foo :: Int -> Cont r Int
foo n = do
    r1 <- fact_cps n
    r2 <- div_10 r1
    r3 <- plus_1 r2
    return r3

class Monad m => MonadCont m where
    callCC :: ((a -> m b) -> m a) -> m a

instance MonadCont (Cont r) where
    -- callCC :: ((a -> Cont r b) -> Cont r a) -> Cont r a
    callCC f = Cont $ \h -> runCont (f (\a -> Cont $ \_ -> h a)) h

fact_cps1 :: Int -> Cont r Int
fact_cps1 0 = return 1
fact_cps1 n = do
          n1 <- fact_cps (n - 1)
          callCC $ \k -> let r = n * n1 
                         in if r > 10000 
                            then k 0
                            else return r

fact_cps2 :: Int -> Cont r Int
fact_cps2 n = do 
           (goto, acc, num) <- callCC $ \k -> let f x y = k (f,x,y)
                                              in return (f, 1, n)
           if num == 1
              then return acc
              else goto (acc * num) (num - 1)

fibs2 :: Int -> Cont r Int
fibs2 0 = return 1
fibs2 1 = return 1
fibs2 n = do
       n1 <- callCC $ \k -> (fibs2 (n - 1))
       n2 <- callCC $ \k -> (fibs2 (n - 2))
       return (n1 + n2)
