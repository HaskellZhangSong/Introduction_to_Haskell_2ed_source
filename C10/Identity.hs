{-# LANGUAGE DeriveFunctor #-}
newtype Identity a = Identity { runIdentity :: a } deriving Functor

instance Applicative Identity where
     pure a = Identity a
     (<*>) (Identity f) (Identity a) = Identity (f a)

instance Monad Identity where
     return a = Identity a
     (Identity m) >>= k  = k m
