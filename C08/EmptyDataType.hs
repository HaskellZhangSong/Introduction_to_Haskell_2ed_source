{-# LANGUAGE EmptyDataDecls, GADTs #-}

data Empty
data NonEmpty

data List a b where
    Nil :: List a Empty
    Cons :: a -> List a b -> List a NonEmpty

safeHead :: List a NonEmpty -> a
safeHead (Cons x _) = x