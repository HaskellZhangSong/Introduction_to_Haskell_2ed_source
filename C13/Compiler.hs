import Data.List
import Control.Monad.Writer
import Control.Monad.State

type Name = Char
type Label = Int

data Exp = Val Int | Var Name | App Op Exp Exp deriving Show

data Op  = Add | Sub | Mul | Div deriving (Show , Eq) 

comexp :: Exp -> Code
comexp (Val int)  = [PUSH int] 
comexp (Var name) = [PUSHV name] 
comexp (App op e1 e2) = comexp e1 ++ comexp e2 ++ [DO op] 

data Prog  = Assign Name Exp
           | If Exp Prog Prog
           | While Exp Prog
           | Seqn [Prog] 
           deriving Show

factorial :: Int -> Prog
factorial n = Seqn [Assign 'A' (Val 1), 
                    Assign 'B' (Val n), 
                    While (Var 'B') (
                    Seqn [Assign 'A' (App Mul (Var 'A') (Var 'B')), 
                          Assign 'B' (App Sub (Var 'B') (Val 1))])] 

type Code  =  [Inst] 
data Inst  =  PUSH Int   
           |  PUSHV Name 
           |  POP Name   
           |  DO Op      
           |  JUMP Label   
           |  JUMPZ Label  
           |  LABEL Label  
              deriving (Show) 
              
type WT a = WriterT Code (State Int) a

fresh :: WT Int
fresh = WriterT $ state (\s -> ((s,mempty),s+1))

mlabel (Assign name expr)    = do tell $ comexp expr
                                  tell [POP name]
                                  
mlabel (If expr prog1 prog2) = do n <- fresh
                                  m <- fresh
                                  tell $ comexp expr
                                  tell [JUMPZ n]
                                  mlabel prog1
                                  tell [JUMP  m]
                                  tell [LABEL n]
                                  mlabel prog2
                                  tell [LABEL m]
mlabel (While expr prog)     = do n <- fresh
                                  m <- fresh
                                  tell [LABEL n]
                                  tell $ comexp expr
                                  tell [JUMPZ m]
                                  mlabel prog
                                  tell [JUMP  n]
                                  tell [LABEL m]
mlabel (Seqn [])   = do tell []
mlabel (Seqn (c:cs))  = do mlabel c
                           mlabel (Seqn cs)

comp :: Prog -> Code
comp prog = snd $ fst $ (runState $ runWriterT $ mlabel prog) 0
