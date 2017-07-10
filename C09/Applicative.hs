import Control.Applicative
class MultiFunctor f where
    fmap0 :: a -> f a -- 等价于可应用函子中的pure函数
    fmap1 :: (a -> b) -> f a -> f b    -- 等价于函子类型类的fmap  
    fmap2 :: (a -> b -> c) -> f a -> f b -> f c
    -- 有了前面的3个函数就可以定义任意多元的fmap函数了
    fmap3 :: (a -> b -> c -> d) -> f a -> f b  -> f c -> f d
    fmap3 h x y z = fmap2 ($) (fmap2 h x y) z
    
    fmap4 :: (a -> b -> c -> d -> e) 
                 -> f a -> f b  -> f c -> f d -> f e
    fmap4 h w x y z = fmap2 ($) (fmap3 h w x y) z
    
    fmap5 :: (a -> b -> c -> d -> e -> g) 
                 -> f a -> f b  -> f c -> f d -> f e -> f g
    fmap5 h v w x y z = fmap2 ($) (fmap4 h v w x y) z
