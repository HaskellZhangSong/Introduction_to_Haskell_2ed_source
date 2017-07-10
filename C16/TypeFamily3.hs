{-# LANGUAGE TypeFamilies #-}

import Data.Vector
import Data.Sequence
data family Array a
data instance Array Int = MkArrayInt (Vector Int)
data instance Array Char = MkArrayChar (Seq Char)