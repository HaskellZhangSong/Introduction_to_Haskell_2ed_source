{-# LANGUAGE FlexibleInstances#-}

class Addition t where
       add :: Int -> t
       
instance Addition Int where
         add x = x

instance (Addition t) => Addition (Int -> t) where
         add i = \x -> add (x+i)
