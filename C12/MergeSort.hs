import Control.Monad.Writer

merge [] xs = xs
merge xs [] = xs
merge (x:xs) (y:ys)
        | x<=y = x : merge xs (y:ys)
        | otherwise = y: merge (x:xs) ys 
        
indent :: Int -> ShowS
indent n = showString (take (4 * n) (repeat ' '))

nl :: ShowS
nl  = showChar '\n'

mergesort :: Int -> [Int] -> Writer String [Int]
mergesort l [] = return []
mergesort l s@[x] = return [x]
mergesort l s@xs = do
  tell $ (indent l.showString "mergesort: ".shows s.nl) ""
  let (a1,a2) = splitAt (length s `div` 2) xs
  tell $ (indent (l+1).showString "merge".shows a1.shows a2.nl) ""
  liftM2 merge (mergesort (l+2) a1) (mergesort (l+2) a2)
