{-# LANGUAGE DeriveDataTypeable,RankNTypes #-}
import Data.Binary
import Data.Data
import Data.Word

myput :: Data a => a -> Put
myput a = let i = (fromIntegral $ constrIndex $ toConstr a) - 1 :: Word8
          in if isAlgType (dataTypeOf a)
                then foldl (>>) (put i) (gmapQ myput a)
                else error "not algebric data type"

getCallBack :: Data a => (forall d.Data d => Get d) -> Get a
getCallBack c = generalCase
            where
                dataType = dataTypeOf ((undefined :: Get b -> b) generalCase)
                generalCase = let index = getWord8 >>= return . fromIntegral
                              in if isAlgType dataType
                                    then index >>= \i -> fromConstrM c (indexConstr dataType (i + 1))
                                    else error "not algebric data type"

myget :: Data a => Get a
myget = getCallBack myget

data A = A Bool | B [Bool] deriving (Show, Data, Typeable)

instance Binary A where
         put = myput
         get = myget
