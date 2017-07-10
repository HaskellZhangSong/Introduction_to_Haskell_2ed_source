-- EightQueens.hs
import Data.List (permutations)
positions 0 n = [[]]
positions k n = [x:xs|  x <- [1..n], xs <- positions (k-1) n]

noSameRow [] = True
noSameRow (x:xs) = (not $ elem x xs) && noSameRow xs

noSameDiag [] = True
noSameDiag xs@(x:xs') = and [abs (i1-i)/=abs (p1-p)|(i,p)<-ip] && noSameDiag xs'
                where (i1,p1):ip = zip [1..] xs

queen n = [xs| xs <- positions n n, noSameRow xs, noSameDiag xs]

positions' 0 n = [[]]
positions' k n = [p:ps| ps<-positions' (k-1) n,p <- [1..n], isSafe p ps]

isSafe p ps = not ((elem p ps) || (sameDiag p ps))
     where sameDiag p ps = any (\(dist,q) -> abs (p-q)==dist) $ zip [1..] ps

queens = positions' 8 8

queens' :: Int -> [[Int]]
queens' n = [xs| xs <- permutations [1..n], noSameDiag xs]