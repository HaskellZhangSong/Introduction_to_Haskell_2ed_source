{-# OPTIONS_GHC -ddump-deriv #-}
import GHC.Generics
data G = GInt {g :: Int} deriving Generic
