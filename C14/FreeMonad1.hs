{-# LANGUAGE GADTs, KindSignatures #-}
import Control.Monad 
import Control.Monad.State

data Interaction :: * -> * where
    Say :: String -> Interaction ()
    Ask :: Interaction String
    Return :: a -> Interaction a
    Bind :: Interaction a -> (a -> Interaction b) -> Interaction b

instance Functor Interaction

instance Applicative Interaction where
    pure = return
    (<*>) = ap

instance Monad Interaction where
    return = Return
    (>>=) = Bind

say = Say
ask = Ask

test1 = do
     say "who are you"
     a <- ask
     say $ "hello " ++ a

run1 :: Interaction a -> IO a
run1 (Say msg) = putStrLn msg
run1 Ask = getLine
run1 (Return x) = return x
run1 (Bind m f) = do x <- run1 m ; run1 (f x)

type Output = [String]
type Input = [String]

run2 :: Interaction a -> State (Input, Output) a
run2 (Say msg) = state $ \(input, write) -> ((), (input, write ++ [msg]))
run2 Ask = state $ \(i:is, write) -> (i , (is, write))
run2 (Return x) = return x
run2 (Bind m f) = do x <- run2 m ; run2 (f x)
