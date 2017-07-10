ones = 1:ones

nature = 0 : map (+1) nature

fibs = (0:1:zipWith (+) fibs (tail fibs))

shorter :: [a] -> [a] -> [a]
shorter xs ys | x < y = xs
              | otherwise ys
              where 
                    x = length xs
                    y = length ys
                    
lazyShorter :: [a] -> [a] -> [a]
lazyShorter xs ys = if short xs ys then xs else ys
            where short [] ys = True
                  short xs [] = False
                  short (x:xs) (y:ys) = short xs ys
