{-# LANGUAGE DeriveDataTypeable, DefaultSignatures,ScopedTypeVariables #-}
import Data.Data

data E1 = A1 | B1 | C1 Int deriving (Show, Typeable, Data)
data E2 = A2 | B2 | C2 | D2 deriving (Show, Typeable, Data)
data E3 = A3 | B3 | C3 | D3 Int deriving (Show, Typeable, Data)

class LastConstr a where
      default lastConstr :: Data a => a
      lastConstr = fromConstr $ last $ dataTypeConstrs (dataTypeOf (undefined :: a))
      lastConstr :: a
      
instance LastConstr E1
instance LastConstr E2
instance LastConstr E3

data E4 = A4 | B4 | C4 | D4 Bool Int Char deriving (Show, Typeable, Data)
