-- Permutation.hs
insert :: a -> [a] -> [[a]]
insert n [] = [[n]]
insert n (n':ns) = (n:n':ns):[n':ns'|ns'<-insert n ns]

permutation [] = [[]]
permutation (x:xs)= concat [insert x permuxs|permuxs<-permutation xs]

