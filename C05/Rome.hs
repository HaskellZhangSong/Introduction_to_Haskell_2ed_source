-- Rome.hs
romeNotation :: [String]
romeNotation= ["M","CM","D","CD","C","XC","L","XL","X","IX","V","IV","I"]

romeAmount :: [Int]
romeAmount = [1000,900, 500, 400, 100, 90, 50, 40, 10, 9, 5, 4, 1]

pair :: [(Int, String)]
pair = zip romeAmount romeNotation

subtrahend  :: Int -> (Int, String)
subtrahend n = head (dropWhile (\(a,_) -> a > n) pair)

convert :: Int -> String
convert 0 = ""
convert n = let (rome, m) = subtrahend n
                in m ++ convert (n-rome)
