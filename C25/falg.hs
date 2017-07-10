{-# LANGUAGE DeriveFunctor #-}

type Algebra f a = f a -> a

newtype Fix f = In { unFix :: f (Fix f) }

cata :: (Functor f) => Algebra f a -> Fix f -> a
cata f = f . fmap (cata f) . unFix

data ListF a s = NilF | ConsF a s deriving Functor

data List a = Nil | Cons a (List a) deriving Functor

lenAlg :: ListF a Int -> Int
lenAlg NilF = 0
lenAlg (ConsF a n) = 1 + n

muList :: List a -> Fix (ListF a)
muList Nil = In NilF
muList (Cons a xs) = In (ConsF a (muList xs))

toList :: Fix (ListF a) -> List a
toList (In NilF) = Nil
toList (In (ConsF a xs)) = Cons a (toList xs)

len :: List a -> Int
len = cata lenAlg . muList

foldr :: Algebra f a -> [a] -> b
foldr = undefined --
