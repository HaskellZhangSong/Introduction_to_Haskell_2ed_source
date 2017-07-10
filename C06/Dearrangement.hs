-- Derangements.hs
import Data.List (delete)

derangements :: [Int] -> [[Int]]
derangements [] = [[]]
derangements l = [x:xs | x <-l,xs<-derangements (delete x l),x /= length l]

derangements' n = map reverse (derangements [1..n])