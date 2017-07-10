{-# LANGUAGE GeneralizedNewtypeDeriving #-}
newtype T a b = NewType (a,b)

newtype Stream a = Cons (a, (Stream a))

newtype Velocity = Velocity Int deriving (Num, Eq)
newtype Weight = Weight Int deriving (Num,  Eq)
newtype Second = Second Int deriving (Num,  Eq)

instance Show Velocity where
         show (Velocity n) = show n ++" m/s"
     
instance Show Weight where
        show (Weight w) = show w ++ " kg"

instance Show Second where
        show (Second 0) = "0 second"
        show (Second 1) = "1 second"
        show (Second n) = show n ++ " seconds"
