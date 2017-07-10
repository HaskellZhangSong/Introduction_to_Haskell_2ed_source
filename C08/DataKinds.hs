-- DataKinds.hs
{-# LANGUAGE GADTs, DataKinds, KindSignatures #-}

data KEmpty = Empty | NonEmpty
data List :: * -> KEmpty -> * where
        Nil :: List a Empty
        Cons :: a -> List a b -> List a NonEmpty