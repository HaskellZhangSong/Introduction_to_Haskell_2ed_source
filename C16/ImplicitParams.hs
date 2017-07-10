{-# LANGUAGE ImplicitParams #-}
import Data.List

sortBy' :: Ord a => (a -> a -> Bool) -> [a] -> [a]
sortBy' f xs = sortBy cmp xs
               where cmp x y = if f x y then LT else GT

sort' :: (?cmp :: a -> a -> Bool) => Ord a => [a] -> [a]
sort' = sortBy' ?cmp

least xs = head (sort' xs)

maxnum = let ?cmp = ((>) :: Int -> Int -> Bool) in least