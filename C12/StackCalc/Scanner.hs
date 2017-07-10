module Scanner where
import Data.Char
import Calculator

scanExp :: String -> [LitOp]
scanExp [] = error "Excepted an expression"
scanExp (' ':ts) = scanExp ts
--scan prefix unary operator
scanExp ('l':'o':'g':ts) = Right Log : scanExp ts
scanExp ('s':'i':'n':ts) = Right Sin : scanExp ts
scanExp ('c':'o':'s':ts) = Right Cos : scanExp ts
scanExp ('s':'q':'r':'t':ts) = Right Sqrt : scanExp ts
scanExp ('+':ts) = Right Posi : scanExp ts
scanExp ('-':ts) = Right Nega : scanExp ts
scanExp ('(':ts) = Right L_Par : scanExp ts
scanExp xs = scanNum xs

scanNum :: String -> [LitOp]
scanNum ('e':ts)     = Left (Const "e") : scanBin ts
scanNum ('p':'i':ts) = Left (Const "pi") : scanBin ts
scanNum xs | null num = error "Excepted a number or constant"
           | otherwise = case rest of
                     ('.': r) -> let (float,r') = span isDigit r 
                                    in Left (Val (read (num ++ "." ++ float) :: Float)) : scanBin r'
                     r        -> Left (Val (read num :: Float)) : scanBin r           
    where (num, rest) = span isDigit xs

scanBin :: String -> [LitOp]
scanBin [] = []
scanBin (' ':ts) = scanBin ts
scanBin ('+':ts) = Right Plus: scanExp ts
scanBin ('-':ts) = Right Minu: scanExp ts
scanBin ('*':ts) = Right Mult: scanExp ts
scanBin ('/':ts) = Right Divi: scanExp ts
scanBin ('^':ts) = Right Power:scanExp ts
scanBin (')':ts) = Right R_Par:scanBin ts
scanBin _ = error "Excepted an infix binary operator"
