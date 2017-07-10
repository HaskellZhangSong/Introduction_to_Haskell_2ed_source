{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies,TypeSynonymInstances,FlexibleInstances #-}

import Control.Monad

left,right :: Int -> (Int , String)
left  x = (x-1, "move left\n")
right x = (x+1, "move right\n")

move i = let (x,str1) = left i in 
         let (y,str2) = left x in 
         (y,str1++str2)  

newtype Writer w a = Writer { runWriter :: (a, w) }
                            deriving Functor

instance (Monoid w) => Monad (Writer w) where
    return x = Writer (x, mempty)
    (Writer (x,v)) >>= f =let (Writer (y, v')) = f x
                            in Writer (y, v `mappend` v')

instance Monoid w => Applicative (Writer w) where
    pure = return
    (<*>) = ap

left', right' :: Int -> Writer String Int
left'  x = Writer (x-1, "move left\n")
right' x = Writer (x+1, "move right\n")

class (Monoid w, Monad m) => MonadWriter w m | m -> w where
    writer :: (a,w) -> m a
    writer ~(a, w) = do
      tell w
      return a
      
    tell   :: w -> m ()
    tell w = writer ((),w)
    listen :: m a -> m (a, w)
    pass   :: m (a, w -> w) -> m a

instance MonadWriter String (Writer String) where
    writer (a,w) = Writer (a,w)
    listen m = return (runWriter m)
    pass m = let ((a,f),s) = runWriter m in writer (a,f s)
    
move' i = do
        x <- left' i
        tell "moved left once!\n gonna move again\n"
        y <- left' x
        return y