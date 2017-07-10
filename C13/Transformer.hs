{-# LANGUAGE DeriveFunctor #-}
import Control.Monad.State (State, state)
import Control.Monad
newtype Identity a = Identity { runIdentity :: a }
                                         deriving Functor
newtype IdentityT m a = IdentityT { runIdentityT :: m a }
                                         deriving Functor

instance (Monad m) => Monad (IdentityT m) where
    return a = IdentityT $ return a
    m >>= k  = IdentityT $ do
                a <- runIdentityT m
                runIdentityT (k a)
                
instance (Monad m) => Applicative (IdentityT m) where
         pure = return
         (<*>) = ap

type IState s a = IdentityT (State s) a

push ::Int ->  IState [Int] ()
push x = IdentityT $ state $ \xs -> ((),x:xs) 

pop :: IState [Int] Int
pop = IdentityT $ state $ \(x:xs) -> (x,xs) 

-- data Maybe a = Nothing | Just a
data MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) } deriving Functor

instance Monad m => Monad (MaybeT m) where
        return x = MaybeT $ return (Just x) 
        MaybeT a >>= f = MaybeT $ do
                               result <- a
                               case result of
                                     Nothing -> return Nothing
                                     Just x -> runMaybeT (f x)

instance (Monad m) => Applicative (MaybeT m) where
         pure = return
         (<*>) = ap

safeHead :: [a] -> MaybeT Identity a
safeHead [] = MaybeT $ Identity Nothing
safeHead (x:xs) = MaybeT $ Identity $ Just x

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
