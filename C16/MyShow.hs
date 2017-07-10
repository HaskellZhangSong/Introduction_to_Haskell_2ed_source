{-# LANGUAGE FlexibleInstances,OverlappingInstances #-}

class MyShow a where
    myShow :: a -> String

instance MyShow Int where myShow = show
instance MyShow Char where myShow = show

instance MyShow a => MyShow [a] where
    myShow [] = "[]"
    myShow xs = "[" ++ showx xs
              where 
                 showx [] = "]"
                 showx [x] = myShow x ++ "]"
                 showx (x:xs) = myShow x ++","++ showx xs

instance MyShow [Char] where
   myShow = show
