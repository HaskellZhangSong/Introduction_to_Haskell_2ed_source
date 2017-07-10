{-# LANGUAGE TypeFamilies,TypeFamilyDependencies #-}

type family F a
type instance F Int = Char
type instance F Bool = Char

type family B a = b | b -> a


