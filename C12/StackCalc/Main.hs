module Main where
import Calculator (calc,inits,Lit (Val),LitOp)
import Scanner (scanExp)
import System.Environment (getArgs)
import Control.Monad.State (evalState)

cal :: String -> LitOp
cal exp = (evalState.calc.scanExp) exp inits

num :: LitOp -> Float
num (Left (Val a)) = a
num _              = error "input error"

calculate = num.cal

main :: IO ()
main = do
        expr <- getArgs
        print $ calculate $ concat expr
