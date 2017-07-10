import Control.Monad.State
import Control.Monad.Trans.Maybe
import Control.Monad.Trans
import Data.Char
import Control.Monad.Trans.Writer

push :: Int -> State [Int] ()
push x = state $ \xs -> ((),x:xs) 

pushMS :: Int -> MaybeT (State [Int]) ()
pushMS x = lift $ push x

isPasswordValid :: String -> Bool
isPasswordValid s = length s >= 8 && check s
        where check :: String -> Bool
              check s = and [f s | f <- map any [isUpper, isLower, isNumber]] 

setPassword:: MaybeT (WriterT [String] IO) ()
setPassword = do
        liftIO $ putStrLn "Please set a password" 
        pass <- liftIO $ getLine
        guard (isPasswordValid pass) 
        lift $ tell [pass] 