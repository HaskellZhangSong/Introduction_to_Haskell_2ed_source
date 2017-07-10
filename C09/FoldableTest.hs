{-# LANGUAGE DeriveFoldable #-}

import Data.Foldable
import Data.Monoid

data Tree a =  Leaf a | Node (Tree a) a (Tree a) 
                      deriving Show

instance Foldable Tree where
   foldMap f (Leaf x) = f x
   foldMap f (Node l n r) = foldMap f l `mappend` f n `mappend` foldMap f r

tree = Node (Leaf 1) 5 (Node (Leaf 0) 2 (Leaf 3))
