{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances,FlexibleContexts #-}

data Shape = Circle Double | Square Double | Rectangle Double Double

instance Eq Shape where
    (==) :: Shape -> Shape -> Bool
    Circle r1 == Circle r2 = r1 == r2
    Square l1 == Square l2 = l1 == l2
    Rectangle a1 b1 == Rectangle a2 b2 = a1 == a2 && b1 == b2
    _ == _ = False
    
data Shape' a = Circle' a | Square' a | Rectangle' a a

instance Eq (Shape' Double) -- 只需要FlexibleInstances
instance Eq (a,a) => Eq (Shape' (a,a)) -- 同时需要两个扩展

class MyShow a where
  myshow :: a -> String
  myshow _ = "default"
  
data Person = P String Int
instance MyShow Person