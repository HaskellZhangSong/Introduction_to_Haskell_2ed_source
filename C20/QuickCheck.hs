-- QuickCheck.hs
import Test.QuickCheck
import Data.List (sort)
prop_even x y = even (x+y) == (even x == even y)

prop_reverseUnit x = reverse [x] == [x]

prop_reverseConcat xs ys = reverse (xs ++ ys) == reverse ys ++ reverse xs

prop_reverseTwice xs = (reverse.reverse) xs == xs

prop_reverseMap f xs = (map f.reverse) xs == (reverse.map f) xs

ordered :: Ord a => [a] -> Bool
ordered []  = True
ordered [x] = True
ordered (x:y:xs) = x<=y && ordered (y:xs)

prop_ordered xs = ordered $ sort xs

prop_headMin xs = head (sort xs) == minimum xs 

prop_headMin' :: Ord a => [a] -> Property
prop_headMin' xs = not (null xs) ==> head (sort xs) == minimum xs

