{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances,UndecidableInstances #-}

import Prelude hiding (Applicative , pure , (<*>), (**))
class Functor f => Applicative (f :: * -> *) where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b

(***) :: (a -> b) -> (c -> d) -> (a, c) -> (b, d)
f *** g = \(x,y) -> (f x, g y)

instance LaxMonoidal f => Applicative f where
    pure x   = fmap (\() -> x) unit
    -- 先得到f (a -> b, a)，再通过(\$)应用二元组中a -> b的函数到a
    f <*> x  = fmap (uncurry ($)) (f ** x) 
    
instance Applicative f => LaxMonoidal f where
    unit = pure ()
    -- fmap (,) :: Functor f => f a ~> f (b -> (a, b))
    -- fmap (,) a :: Functor f => f (b -> (a, b))
    a ** b =  (fmap (,) a) <*> b

class Functor f => LaxMonoidal f where
    unit :: f ()   -- 对应 $i$
    (**) :: f a -> f b -> f (a,b)  -- 对应$\phi$，等价于(f a, f b) -> f (a,b)
