{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes, TypeInType #-}

import GHC.Types
import GHC.Prim

f :: Int -> Int#
f (I# i) = undefined

-- undefined' :: forall (r :: RuntimeRep) (a :: TYPE r). a
undefined' = undefined

-- f' :: Int -> Int#
-- f' (I# i) = undefined'

g :: Int -> Int#
g (I# i) = i

id' :: forall (v :: RuntimeRep) (a :: TYPE r) . a -> a
id' x = x
