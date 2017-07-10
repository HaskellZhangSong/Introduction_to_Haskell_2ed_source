{-# LANGUAGE TemplateHaskell, DeriveGeneric #-}
module QuickCheck1  where
import Data.DeriveTH
import Data.Derive.Arbitrary
import Test.QuickCheck
import GHC.Generics
import Text.Show.Functions

data Exp = Val Int
         | Add Exp Exp
         deriving (Show, Eq, Generic)

eval :: Exp -> Int
eval (Val x) = x
eval (Add x y) = eval x + eval y

derive makeArbitrary ''Exp
instance CoArbitrary Exp

prop_foo :: Exp -> Exp -> Bool
prop_foo x y = eval x /= eval y

prop_bar :: (Exp -> Int) -> Exp -> Bool
prop_bar f e = f e > 0

return []
check = $verboseCheckAll 
