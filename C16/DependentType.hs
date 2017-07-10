

data Vec a (n :: Nat) where
         Nil :: Vec a Z
         Cons :: a -> Vec a n -> Vec a (S n)

deriving instance Show a => Show (Vec a n)

vhead ::  Vec a (S n) -> a
vhead (Cons a v) = a

vtail :: Vec a (S n) -> Vec a n
vtail (Cons x xs) = xs

append :: Vec a n -> Vec a m -> Vec a (n + m)
append (Cons x xs) ys = Cons x (append xs ys)
append Nil       ys = ys

toList :: Vec a n -> [a]
toList Nil = []
toList (Cons x xs) = x : toList xs