{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies,FlexibleInstances #-}

import Control.Monad

newtype Reader r a = Reader { runReader :: r -> a }
                            deriving Functor

instance Monad (Reader r) where
        return a = Reader $ \_ -> a
        m >>= k  = Reader $ \r -> runReader (k (runReader m r)) r

instance Applicative (Reader r) where
        pure = return
        (<*>) = ap
        
readLen :: Reader [a] Int
readLen = Reader $ \r -> length r

class (Monad m) => MonadReader r m | m -> r where
    ask   :: m r
    local :: (r -> r) -> m a -> m a

instance MonadReader r (Reader r) where
    ask = Reader id
    local f m = Reader $ runReader m . f

test :: Reader [Int] [Int]
test = do
         xs <- local (map (+1)) ask
         ys <- ask
         return ys

withReader :: (r' -> r) -> Reader r a -> Reader r' a
withReader f m = Reader $ runReader m . f

mapReader :: (a -> b) -> Reader r a -> Reader r b
mapReader f m = Reader $ f . runReader m