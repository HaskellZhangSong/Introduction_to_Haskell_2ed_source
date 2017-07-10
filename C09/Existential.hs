-- Existential.hs
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs, KindSignatures, ConstraintKinds,CPP #-}
import Data.Constraint

data Showy = forall a. (Show a) => Showy a 

instance Show Showy where
        show (Showy a) = show a

showType :: [Showy]
showType = [Showy (1::Int), Showy "String", Showy 'c']

data Shape = forall a. (HasArea a) => Shape a 

class HasArea t where
    area :: t -> Double
    
instance HasArea Shape where
        area (Shape a) = area a
        
data Some :: (* -> Constraint) -> * where
    Some :: c a => a -> Some c