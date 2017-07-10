import Data.STRef
import Control.Monad.ST

factorial :: Int ->  STRef s Int -> ST s Int
factorial n accRef = do
          numRef <- newSTRef n
          num <- readSTRef numRef
          if num < 1
              then readSTRef accRef
              else do
                 acc <- readSTRef accRef
                 writeSTRef accRef (acc * n)
                 writeSTRef numRef (num - 1)
                 factorial (num - 1) accRef
              
fact :: Int -> Int
fact n = runST $ do
            accRef <- newSTRef 1
            factorial n accRef
