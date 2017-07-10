-- Test.hs
{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
class C a where
    foo :: a -> IO ()

class A a where
instance A Bool
instance A Int

instance (A a) => C a where
    foo = undefined

class B a where
instance B Char
instance B Double

-- instance (B a) => C a where
