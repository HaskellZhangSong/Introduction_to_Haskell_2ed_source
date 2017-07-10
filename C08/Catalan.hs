data Tree = Leaf | Node Tree Tree deriving Show

trees :: Int -> [Tree]
trees 0  = [Leaf]
trees n  = [Node lt rt | l <-[0..(n-1)], lt <- trees l , rt <- trees (n-1-l)]

brace :: Tree -> String
brace Leaf = ""
brace (Node l r) = '(':brace l ++")" ++ brace r
