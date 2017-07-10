{-# LANGUAGE DeriveFunctor #-}
import Control.Monad

newtype State s a = State { runState :: s -> (a,s) } 
                            deriving Functor

newtype Reader r a = Reader { runReader :: r -> a }
                            deriving Functor
                            
instance Monad (State s) where
        return x = State $ \s -> (x,s)
        (State h) >>= f = State $ \s -> let (a, newState) = h s
                                            (State g) = f a
                                        in g newState
                                        
instance Applicative (State s) where
    pure = return
    (<*>) = ap

evalState :: State s a -> s -> a
evalState m s = fst (runState m s)

evaluate :: State s a -> Reader s a
evaluate s =  Reader $ \e -> evalState s e

readOnly :: Reader s a -> State s a
readOnly r = State $ \s -> (runReader r s, s)

data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving (Show,Eq)

labelTree :: Tree a -> Tree (a,Int)
labelTree t = fst $ ntAux t 0

ntAux :: Tree a -> Int  -> (Tree (a,Int),Int)
ntAux (Leaf a) n = (Leaf (a,n),n+1)
ntAux (Node l a r) n = let (nn,n')   = ((a,n),n+1) in 
                       let (ln,n'')  = ntAux l n'  in
                       let (rn,n''') = ntAux r n'' in 
                       (Node ln nn rn, n''') 

test :: Tree Int
test = Node (Node (Leaf 5) 3 (Leaf 2)) 7 (Leaf 9)
