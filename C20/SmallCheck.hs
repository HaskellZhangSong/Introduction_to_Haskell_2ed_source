{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, DeriveGeneric #-}

import Test.SmallCheck
import Test.SmallCheck.Series

import GHC.Generics
import Data.Functor.Identity

data Exp = Val Bool
         | And Exp Exp
         | Or  Exp Exp
         deriving (Show, Eq, Generic)

eval :: Exp  -> Bool
eval (Val b) = b
eval (And a b) = eval a && eval b
eval (Or  a b) = eval a || eval b

instance Monad m => Serial m Exp

prop_1 :: Monad m => Property m
prop_1 = forAll $ \x -> eval x == True || eval x == False

x6 :: Monad m => Property m
x6 = existsUnique $ \x -> (x - 6) > (0 :: Int)

prop_assoc :: Monad m => Property m
prop_assoc = forAll $ \ x y z -> ((x + y) + z) == (x + (y + (z :: Int)))

prop_assoc1 :: Monad m => Property m
prop_assoc1 = forAll $ \ x y z -> ((x + y) + z) == (x + (y - (z::Int)))
