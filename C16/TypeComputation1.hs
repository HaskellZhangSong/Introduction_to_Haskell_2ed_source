-- TypeComputation1.hs
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies,
             FlexibleInstances, UndecidableInstances        #-}
data Zero
data Succ a

class Add a b ab | a b -> ab, a ab -> b
instance Add Zero b b
instance (Add a b ab) => Add (Succ a) b (Succ ab)
