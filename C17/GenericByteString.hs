{-# LANGUAGE TypeFamilies, UndecidableInstances, OverloadedStrings #-}
import GHC.Generics
import GHC.Word
import Data.ByteString

instance Generic ByteString where
    type Rep ByteString = Rep [Word8]
    from bs = from (unpack bs)
    to w = pack $ (to w)

abc = from ("abc":: ByteString)
