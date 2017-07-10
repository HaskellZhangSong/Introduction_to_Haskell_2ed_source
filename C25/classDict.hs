{-# LANGUAGE KindSignatures, ConstraintKinds, GADTs,TypeOperators,RankNTypes  #-}

import Control.Category
import GHC.Prim (Constraint) -- Constraint is in GHC.Types after GHC8

data OrdDict a where
     OrdDict :: Ord a => OrdDict a

data Dict (p :: Constraint) where
     Dict :: p => Dict p

-- Sub :: (p => Dict q) -> p :- q
newtype p :- q = Sub (p => Dict q)

(\\) :: p => ((q => r) -> (p :- q) -> r)
r \\ (Sub Dict) = r

trans :: (b :- c) -> (a :- b) -> (a :- c)
trans f g = Sub $ (Dict \\ f) \\ g

refl :: a :- a
refl = Sub Dict

instance Category (:-) where
     id = refl
     (.) = trans 



