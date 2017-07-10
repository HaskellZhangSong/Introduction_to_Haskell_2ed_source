import Data.List (permutations)

data Exp = Val Double
         | Plus Exp Exp 
         | Sub  Exp Exp 
         | Mult Exp Exp 
         | Div  Exp Exp deriving (Show,Eq)
         
eval :: Exp -> Double
eval (Val  a )    = a
eval (Plus a b ) = eval a + eval b
eval (Sub  a b ) = eval a - eval b
eval (Mult a b ) = eval a * eval b
eval (Div  a b ) = eval a / eval b

showExp :: Exp -> String
showExp (Val a)    = show a
showExp (Plus a b) = "("++showExp a ++ "+" ++ showExp b++")"
showExp (Sub  a b) = "("++showExp a ++ "-" ++ showExp b++")"
showExp (Mult a b) = "("++showExp a ++ "*" ++ showExp b++")"
showExp (Div  a b) = "("++showExp a ++ "/" ++ showExp b++")"

divide :: [a] -> [([a],[a])]
divide xs = [(take n xs ,drop n xs)| n <- [1..(length xs -1)]] 

buildExpressions :: ([Exp],[Exp]) -> [Exp]
buildExpressions (es1,es2) = [op e1 e2 |e1<-es1, e2<- es2, 
                                        op <- [Plus, Sub, Mult, Div]]
                                        
toExpressions :: [Double] -> [Exp]
toExpressions [] = []
toExpressions [x] = [Val x]
toExpressions xs = concat [buildExpressions (toExpressions l,
                     toExpressions r)| (l,r) <- divide xs ]
                     
generate :: [Double] -> [Exp]
generate ns = concatMap toExpressions (permutations ns)

twentyfour :: [Double] -> [String]
twentyfour ns = [showExp x | x <- generate ns, eval x == 24.0 ]