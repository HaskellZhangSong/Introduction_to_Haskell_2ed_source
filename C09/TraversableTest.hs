{-# LANGUAGE DeriveFoldable,DeriveFunctor #-}
data Tree a =  Leaf a | Node (Tree a) a (Tree a) 
                                   deriving (Show,Foldable,Functor)

instance Traversable Tree where
    -- traverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
    traverse f (Leaf a) = Leaf <$> f a
    traverse f (Node l a r) = Node <$> traverse f l <*> f a <*> traverse f r
    -- sequenceA :: Applicative f => Tree (f a) -> f (Tree a)
    sequenceA (Leaf a) = Leaf <$> a
    sequenceA (Node l a r) = Node <$> (sequenceA l) <*> a <*> (sequenceA r)

tree0,tree1 :: Tree Double
tree0 = (Node (Leaf 0) 1 (Leaf 2))
tree1 = (Node (Leaf 1) 2 (Leaf 3))

reciprocal  :: (Eq b, Fractional b, Traversable t) => t b -> Maybe (t b)
reciprocal = traverse (\x -> if x == 0 then Nothing else Just (1/x))
