data Zipper a = Zipper [a] a [a] deriving Show

fromList :: [a] -> Zipper a
fromList (x:xs) = Zipper [] x xs
fromList _      = error "empty!"

next :: Zipper a -> Zipper a
next (Zipper ys y (x:xs)) = Zipper (y:ys) x xs
next z  = z

prev :: Zipper a -> Zipper a
prev (Zipper (y:ys) x xs) = Zipper ys y (x:xs)
prev z  = z 

data Tree a = Leaf a | Node a (Tree a) (Tree a)
data Accumulate a = Empty | L (Accumulate a) a (Tree a) 
                          | R (Accumulate a) a (Tree a) 


type Zipper a = (Tree a, Accumulate a)

right,left,up :: Zipper a -> Zipper a
right (Node n l r, a) = (r, R a n l)
right a = a

left (Node n l r, a) = (l, L a n r)
left a = a

up (t, R a n l) = (Node n l t, a)
up (t, L a n r) = (Node n t r, a)
up z@(t, Empty )= z