{-# LANGUAGE DeriveDataTypeable #-}
import Data.Typeable

data Person = Person String Bool deriving (Show, Typeable)

equalTypes :: (Typeable a , Typeable b) => a -> b -> Bool
equalTypes a b = if typeOf a == typeOf b then True else False
