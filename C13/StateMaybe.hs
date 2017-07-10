import Control.Monad.State
import Control.Monad.Trans.Maybe

pushSM :: Int -> StateT [Int] Maybe ()
pushSM x = StateT $ \xs -> Just ((),x:xs) 

popSM ::  StateT [Int] Maybe Int
popSM = StateT $ \xs -> case xs of
                           [] -> Nothing
                           (x:xs) -> Just (x,xs) 

pushMS :: Int -> MaybeT (State [Int]) ()
pushMS x = MaybeT $ state $ \xs -> (Just (),x:xs) 

popMS :: MaybeT (State [Int]) Int
popMS = MaybeT $ state $ \xs -> case xs of
                                    [] -> (Nothing, xs) 
                                    (y:ys) -> (Just y, ys) 