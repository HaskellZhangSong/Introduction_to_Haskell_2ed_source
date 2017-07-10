{-# LANGUAGE DeriveFunctor #-}
import Control.Monad

newtype WriterT w m a = WriterT { runWriterT :: m (a, w) }
                    deriving Functor

instance (Monoid w, Monad m) => Applicative (WriterT w m) where
  pure = return
  (<*>) = ap

instance (Monoid w, Monad m) => Monad (WriterT w m) where
  return a = WriterT $ return (a, mempty)
  m >>= k = WriterT $ do
    (a, w) <- runWriterT m
    (b, w') <- runWriterT (k a)
    return (b, w `mappend` w')
    

