import Control.Monad.State
import Control.Monad.Trans.Maybe

push :: Int -> State [Int] ()
push x = state $ \xs -> ((),x:xs) 

pop :: State [Int] (Maybe Int) 
pop = state $ \xs -> case xs of
                        [] -> (Nothing,[]) 
                        (x:xs) -> (Just x, xs) 

stack :: State [Int] ()
stack = do
        push 5
        pop
        pop
        push 4

stack1 :: State [Int] (Maybe Int) 
stack1 = do
        push 5
        a<-pop
        case a of
            Nothing -> return Nothing
            Just a  -> return (Just (a+1)) 

stackMS1 :: MaybeT (State [Int]) Int
stackMS1 = do
        pushMS 5
        i <- popMS
        return (i+1) 

pushMS :: Int -> MaybeT (State [Int]) ()
pushMS x = MaybeT $ state $ \xs -> (Just (),x:xs) 

popMS :: MaybeT (State [Int]) Int
popMS = MaybeT $ state $ \xs -> case xs of
                                    [] -> (Nothing, xs) 
                                    (y:ys) -> (Just y, ys) 
