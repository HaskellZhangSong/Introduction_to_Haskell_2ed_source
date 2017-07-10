-- Kind.hs
{-# LANGUAGE GADTs, KindSignatures, FlexibleInstances #-}
data T :: * -> * where
      NIL :: T a
      CONS :: a -> T a -> T a

data AbsTree k a = Leaf a | Node (k (AbsTree k a))

data Tree :: (* -> *) -> * -> * where
      L :: a -> Tree k a
      N :: k (Tree k a) -> Tree k a

type RoseTree a = Tree [] a

instance Show a => Show (RoseTree a) where
     show (L a) = show a	
     show (N tree) = show tree

test :: RoseTree Int
test = N [N[L 5, L 8, N [L 1 , L 2]], N[L 3]]