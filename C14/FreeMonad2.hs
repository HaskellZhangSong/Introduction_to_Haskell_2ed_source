{-# LANGUAGE GADTs , KindSignatures #-}
import Control.Monad

data Interaction :: * -> * where
    Return :: a -> Interaction a
    Say :: String -> (() -> Interaction b) -> Interaction b
    Ask :: (String -> Interaction b) -> Interaction b

instance Functor Interaction where
    fmap f (Return a) = Return (f a)
    fmap f (Say str fu) = Say str (\() -> fmap f (fu ()))
    fmap f (Ask fs) = Ask (\str -> fmap f (fs str))
    
instance Applicative Interaction where
    pure = return
    (<*>) = ap

instance Monad Interaction where
    return = Return 
    Return x >>= f  = f x
    Say msg k >>= f = Say msg ((>>=f).k)
    Ask k >>= f = Ask ((>>=f).k)

say :: String -> Interaction ()
say msg = Say msg Return
ask :: Interaction String
ask = Ask Return

run :: Interaction a -> IO a
run (Return x) = return x
run (Say msg k) = putStrLn msg >>= run.k
run (Ask k) = getLine >>= run.k

run2 :: Interaction a -> [String] -> [String]
run2 (Return _) is = []
run2 (Say msg k ) is = [msg] ++ run2 (k ()) is
run2 (Ask k) (i:is) = run2 (k i) is

test1 :: Interaction ()
test1 = do
     say "who are you"
     a <- ask
     say $ "hello " ++ a
