{-# LANGUAGE DeriveFunctor,FlexibleInstances,Arrows,TupleSections #-}
import Control.Arrow hiding (Kleisli(..))
import Control.Category hiding (Kleisli(..))

import Prelude hiding (id, (.))

newtype SF a b = SF {runSF :: [a] -> [b]} deriving Functor

instance Applicative (SF b) where
   pure x = SF $ \a -> map (const x) a
   -- f ::  [b] -> [a -> b1]
   -- x ::  [b] -> [a]
   -- res :: [b] -> [b1]
   (<*>) (SF f) (SF x) = SF $ \bs -> zipWith ($) (f bs) (x bs)

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

newtype Kleisli m a b = Kleisli { runKleisli :: a -> m b }

class Applicative f => Static f where
  delay :: Kleisli f a b -> Kleisli f () (a -> b)
  
instance Static f => Category (Kleisli f) where
  id = Kleisli pure
  -- bfc :: Kleisli (b -> f c)
  -- afb :: Kleisli (a -> f b)
  -- res :: Kleisli (a -> f c)
  (.) bfc afb = Kleisli $ \a -> let fab = (runKleisli $ delay afb) ()
                                    fbc = (runKleisli $ delay bfc) ()
                                in fbc <*> (fab <*> pure a)

instance Static f => Arrow (Kleisli f) where
  arr f = Kleisli $ \a -> pure (f a)
  -- bfc :: Kleisli f b c
  -- res :: Kleisli f (b,d) (c,d)
  first bfc = Kleisli $ \(b,d) -> let fbc = (runKleisli $ delay bfc) ()
                                  in pure (,d) <*> (fbc <*> pure b)
