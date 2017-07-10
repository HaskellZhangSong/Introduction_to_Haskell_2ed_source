{-# LANGUAGE NoImplicitPrelude #-}
import Prelude (undefined, Either (..))

(.) :: (b -> c) -> (a -> b) -> (a -> c)
(.)  f g x = f (g x)

(*) :: (a1 -> b1) -> (a2 -> b2) -> (a1,a2) -> (b1,b2)
(*) f g (a1,a2) = (f a1, g a2)

(<.>) :: (a -> b) -> (a -> c) -> a -> (b,c)
(<.>) f g a =  (f a, g a)

pi1 :: (a,b) -> a
pi1 (a,b) = a

pi2 :: (a,b) -> b
pi2 (a,b) = b

data Pair a b = Pair a b

f :: Pair b c -> b
f (Pair a b) = a

g :: Pair b c -> c
g (Pair a b) = b

h :: a -> Pair b c
h = undefined

either  :: (a -> c) -> (b -> c) -> Either a b -> c
either f _ (Left x)     =  f x
either _ g (Right y)    =  g y
