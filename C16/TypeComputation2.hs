-- TypeComputation2.hs
{-# LANGUAGE KindSignatures,GADTs,TypeOperators,DataKinds,
            UndecidableInstances,StandaloneDeriving,
            ExistentialQuantification, TypeFamilies #-}

data Nat = Z | S Nat deriving (Eq, Show)

type family  (a :: Nat) + (b :: Nat) :: Nat where
     Z   + m = m
     S n + m = n + S m

type family   (n :: Nat) * (m :: Nat) :: Nat where
     Z   * m = Z
     S n * m = (n * m) + m



data Vec a (n :: Nat) where
         Nil :: Vec a Z
         Cons :: a -> Vec a n -> Vec a (S n)

deriving instance Show a => Show (Vec a n)

vhead ::  Vec a (S n) -> a
vhead (Cons a v) = a

vtail :: Vec a (S n) -> Vec a n
vtail (Cons x xs) = xs
{-
append :: Vec a n -> Vec a m -> Vec a (n + m)
append (Cons x xs) ys = Cons x (append xs ys)
append Nil       ys = ys
-}
toList :: Vec a n -> [a]
toList Nil = []
toList (Cons x xs) = x : toList xs

data SNat (n :: Nat) where
     SZ :: SNat Z
     SS :: SNat n -> SNat (S n)
     
fromList :: SNat n -> [a] -> Vec a n
fromList SZ [] = Nil
fromList (SS n) (x:xs) = Cons x (fromList n xs)
fromList _ _ = error "size not matched"

replicate' :: SNat n -> a -> Vec a n
replicate' SZ _ = Nil
replicate' (SS n) a = Cons a (replicate' n a)
