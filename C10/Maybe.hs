{-# LANGUAGE DeriveFunctor #-}
import Prelude hiding (Maybe, Just, Nothing)

data Identity a = Identity a
data Maybe a    = Just     a  |   Nothing deriving Functor

instance Applicative Maybe where
        pure = Just
        Just f <*> Just a = Just (f a)
        _ <*> _ = Nothing
        
instance Monad Maybe where
        return = Just
        (Just a) >>= f = f a
        Nothing >>= _ = Nothing
        fail _ = Nothing

data Exp = Lit Integer
         | Add Exp Exp
         | Sub Exp Exp
         | Mul Exp Exp
         | Div Exp Exp
         deriving (Show)

safeEval (Add e1 e2) = do
                   n1 <- safeEval e1
                   n2 <- safeEval e2
                   return (n1+n2)
