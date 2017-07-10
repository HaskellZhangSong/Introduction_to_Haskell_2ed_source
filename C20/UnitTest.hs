-- UnitTest.hs
import Test.HUnit
import Data.List

foo _ = (1,True)
test1 = TestCase (assertEqual "for (foo 3)" (1,True) (foo 3))


qsort :: Ord a => [a] -> [a]
qsort []     = []
qsort (p:xs) = qsort lesser ++ [p] ++ qsort greater
  where
    lesser  = [ y | y <- xs, y > p ]
    greater = [ y | y <- xs, y >= p ]

test2 :: Test
test2 = TestCase $ assertBool "for qsort" (let xs = [6,4,7,8,1,2,9,4]
                                           in qsort xs == sort xs)


