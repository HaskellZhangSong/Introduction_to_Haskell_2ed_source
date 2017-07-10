{-# LANGUAGE TypeFamilies #-}
class Collection ce where
    type Element ce :: *
    empty :: ce
    insert :: Element ce -> ce -> ce
    member :: Element ce -> ce -> Bool
    
instance Eq a => Collection [a] where
    type Element [a] = a
    empty = []
    insert x xs = x:xs
    member x xs = elem x xs
