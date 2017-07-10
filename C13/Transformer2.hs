{-# LANGUAGE DeriveFunctor #-}
import Control.Monad
import Data.Functor.Identity
--newtype State s a = State { runState :: s -> (s,a) }
newtype StateT s m a = StateT { runStateT :: s -> m (a,s) } deriving Functor

instance (Monad m) => Monad (StateT s m) where
    return a = StateT $ \s -> return (a, s) 
    m >>= k  = StateT $ \s -> do
        (a, s') <- (runStateT m) s
        runStateT (k a) s'
        
instance (Monad m) => Applicative (StateT s m) where
         pure = return
         (<*>) = ap

type State s a = StateT s Identity a

push :: Int -> State [Int] ()
push x = StateT $ \xs -> Identity ((), x:xs) 

pop :: State [Int] Int
pop = StateT $ \(x:xs) -> Identity (x,xs) 