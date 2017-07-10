-- insertation sort
insert :: Ord a => a -> [a] -> [a]
insert x []       = [x]                     
insert x (y:ys)  | x < y = x:y:ys           
                 | otherwise = y:insert x ys
                 
insertionSort :: Ord a => [a] -> [a]
insertionSort []      = []                                
insertionSort (x:xs) = insert x (insertionSort xs)  

-- bubble sort
swaps :: Ord a => [a] -> [a]
swaps []  = []
swaps [x] = [x]
swaps (x1:x2:xs) | x1 > x2   = x2: swaps (x1:xs)
                 | otherwise = x1: swaps (x2:xs) 
                 
fix  :: Eq a => (a -> a) -> a -> a
fix f x = if x == x' then x else fix f x'
                           where x' = f x
                           
bubbleSort :: Ord a => [a] -> [a]
bubbleSort xs = fix swaps xs

bubbleSort'' :: Ord a => [a] -> [a]
bubbleSort'' [] = []
bubbleSort'' xs = bubbleSort'' initialElements ++ [lastElement]
                 where swappedxs        = swaps xs
                       initialElements = init swappedxs
                       lastElement     = last swappedxs
                       
-- Selection sort
delete :: Eq a => a -> [a]-> [a]
delete _ [] = []
delete x (l:ls) | x == l = ls
                | otherwise = l:delete x ls
                
selectionSort []      = []
selectoinSort xs = mini : selectionSort xs'
        where mini = minimum xs
              xs'  = delete mini xs
              
-- Quick sort
quickSort :: Ord a => [a] -> [a]
quickSort [] = [] 
quickSort (x:xs) = quickSort mini ++ [x] ++ quickSort maxi
       where mini = filter (<x) xs
             maxi = filter (>=x) xs

-- Merge sort
merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) | x > y = y:merge (x:xs) ys
                    | otherwise = x:merge xs (y:ys)
                    
msort :: Ord a => [a] -> [a]
msort xs = merge (msort x1) (msort x2)
  where
    (x1,x2)  = halve xs
    halve xs = (take l xs, drop l xs)
    l        = (length xs) `div` 2