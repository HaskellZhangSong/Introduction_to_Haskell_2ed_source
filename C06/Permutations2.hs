import Data.List (delete)

permutation :: Eq a => [a] -> [[a]]
permutation [] = [[]]
permutation xs = [y:ys|  y<-xs, ys<-permutation (delete y xs)]

permutation' :: Eq a => Int -> [a] -> [[a]]
permutation' 0 _ = [[]]
permutation' n l = [x:xs | x<-l , xs <- permutation' (n-1) (delete x l)]
