{-# LANGUAGE GADTs #-}

data Exp a where
    ValInt   :: Int -> Exp Int
    ValBool  :: Bool -> Exp Bool
    Add      :: Exp Int -> Exp Int -> Exp Int
    Equa     :: Exp Int -> Exp Int -> Exp Bool
    
eval :: Exp a -> a
eval (ValInt  i) = i
eval (ValBool b) = b
eval (Add e1 e2) = eval e1 + eval e2
eval (Equa e1 e2) = eval e1 == eval e2

data Tree a where
    Leaf :: a -> Tree Int
    Branch :: Tree a -> Tree a -> Tree Int