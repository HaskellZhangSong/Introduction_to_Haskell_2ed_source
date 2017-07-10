{-# LANGUAGE DeriveFunctor #-}
data Tree a = Leaf | Node a (Tree a) (Tree a) deriving (Show, Eq, Functor)

data Container a = Container a Int
instance Functor Container where
    fmap g (Container x i) = Container (g x) (i+1)