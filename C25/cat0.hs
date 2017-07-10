{-# LANGUAGE ConstraintKinds, DataKinds,KindSignatures,PolyKinds,TypeFamilies #-}
import Data.Constraint  

class Category (cat :: k -> k -> *) where
    type Object cat (a :: k) :: Constraint
    id :: Object cat a => cat a a
    (.) :: (Object cat a, Object cat b, Object cat c) =>
                                        cat b c -> cat a b -> cat a c

instance Category (->) where
    type Object (->) (a :: *) = ()
    id x = x
    (.) g f x = g (f x)

data Void
data Unit = Unit

absurd :: Void -> a
absurd = absurd

unit :: a -> Unit
unit _ = Unit

data V
data U = U

vphi :: V -> Void
vphi = vphi

vpsy :: Void -> V
vpsy = absurd

uphi :: U -> Unit
uphi U = Unit

upsy :: Unit -> U
upsy Unit = U
