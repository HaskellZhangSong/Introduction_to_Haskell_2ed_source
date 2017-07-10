{-# LANGUAGE FlexibleInstances,Arrows #-}
import Control.Monad
import Control.Applicative
import Control.Arrow hiding (Kleisli)
import Control.Category
import Prelude hiding ((.), id)

newtype Kleisli m a b = Kleisli { runKleisli :: a -> m b }

instance Monad m => Category (Kleisli m) where
    id = Kleisli return
    (Kleisli f) . (Kleisli g) = Kleisli (f <=< g)

instance Monad m => Arrow (Kleisli m) where
    arr f = Kleisli (return . f)
    first (Kleisli f) = Kleisli (\ ~(b,d) -> f b >>= \c -> return (c,d))
    second (Kleisli f) = Kleisli (\ ~(d,b) -> f b >>= \c -> return (d,c))

instance {-# OVERLAPPABLE #-} (Arrow arr) => Functor (arr a) where
  -- f :: b -> c
  -- ab :: arr a b
  -- res :: arr a c
  fmap f ab = proc c -> do
                b <- ab -< c
                returnA -< f b

instance {-# OVERLAPPABLE #-} (Arrow arr) => Applicative (arr a) where
  pure x = arr (\a -> x)
  -- f :: arr a (b -> c)
  -- x :: arr a b
  -- res :: arr a c
  (<*>) f x =  proc a -> do
                bc <- f -< a
                b  <- x -< a
                returnA -< bc b

instance {-# OVERLAPPABLE #-} ArrowApply a => Monad (a ()) where
  return x = arr $ \_ -> x
  -- x :: a () b
  -- f :: b -> a () c
  -- res :: a () c
  x >>= f = proc () -> do
               b <- x -< ()
               ac <- arr f -< b
               c <- ac -<< ()
               returnA -< c

  x >>= f = x >>> (arr $ \x -> let h = f x in (h, ())) >>> app

instance (ArrowApply a, ArrowPlus a) => Alternative (a ()) where
   empty = zeroArrow
   x <|>  y = (x <+> y)

foo :: Kleisli IO String ()
foo = proc str -> do
    ln <- Kleisli (\() -> getLine) -< ()
    () <- Kleisli putStrLn -< ln
    returnA -< ()
