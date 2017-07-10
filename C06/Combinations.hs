-- Combinations.hs
import Data.List (tails)

powerSet = choice (\x -> [True,False])

choice :: (a -> [Bool]) -> [a] -> [[a]]
choice _ [] = [[]]
choice f (x:xs) =[if choose then x:ys else ys|choose <- f x , ys<- choice f xs]

powerSet' :: [a] -> [[a]]
powerSet' [] = [[]]
powerSet' (x:xs) = [x:r | r <- powerSet' xs ] ++ powerSet' xs

combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations n xs = [y:ys | y:xs'<- tails xs, ys <- combinations (n-1) xs']
