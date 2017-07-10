import Control.Monad.Reader
import Data.List (lookup)

data Exp = Val Int
         | Var String
         | Add Exp Exp 
         | Decl Bind Exp deriving (Show ,Eq)

type Bind = (String,Int)         
type Env = [Bind]

updateEnv :: Bind -> Env -> Env
updateEnv = (:)

resolve :: Exp -> Reader Env (Maybe Exp)
resolve (Val i ) = return (Just (Val i))
resolve (Var s ) = do
                env <- ask
                case lookup s env of
                        Nothing -> return Nothing
                        Just v  -> return (Just (Val v))

resolve (Add e1 e2) = do
                    re1 <- resolve e1
                    case re1 of
                        Nothing -> return Nothing
                        Just a  -> do 
                                re2 <- resolve e2
                                case re2 of
                                    Nothing -> return Nothing
                                    Just b -> return (Just (Add a b))

resolve (Decl b e) = local (updateEnv b) (resolve e)

-- let x=3 in let y=5 in x + (y+6)
test1 :: Exp
test1 = Decl ("x",3) (Decl ("y",5) (Add (Var "x") (Add (Var "y") (Val 6))))

--let x = 2 in x + let x = 3 in x
test2 :: Exp
test2 = Add (Decl ("x",2) (Var "x")) (Decl ("x",3) (Var "x"))
