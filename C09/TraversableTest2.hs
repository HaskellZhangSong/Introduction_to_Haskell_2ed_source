import Data.Functor.Identity
import Data.Functor.Constant
-- import Data.Traversable

fmap' :: Traversable t => (a -> b) -> t a -> t b
fmap' f x = runIdentity $ traverse (Identity . f) x

foldMap' :: (Traversable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f x = getConstant $ traverse (Constant . f) x

newtype Const a b = Const {getConst :: a} 

instance Functor (Const a) where
    fmap f (Const x) = Const x

instance (Monoid a) => Applicative (Const a) where
    pure _ = Const mempty
    Const x <*> Const y = Const (x `mappend` y)
    
instance Foldable (Const a) where
    -- foldMap :: Monoid a => (a -> m) -> Const a1 a -> m
    foldMap f (Const x) = mempty

    
instance Traversable (Const a) where
    -- traverse :: Applicative f => (a1 -> f b) -> Const a a1 -> f (Const a b)
    traverse f (Const x) = pure (Const x)