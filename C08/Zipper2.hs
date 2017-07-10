data Tree a = Leaf a | Node a (Tree a) (Tree a)

data Branch a = R a (Tree a) | L a (Tree a)

type Zipper a = (Tree a, [Branch a])

right, left, up :: Zipper a -> Zipper a

right (Node n l r, t) = (r, R n l: t)
right z@(Leaf a, t) = z

left (Node n l r, t) = (l, L n r :t)
left z@(Leaf a, t) = z

up (r, (R n l: t)) = (Node n l r ,t)
up (l, (L n r: t)) = (Node n l r ,t)
up z@(t, []) = z