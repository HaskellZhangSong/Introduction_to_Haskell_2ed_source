{-# LANGUAGE DeriveFunctor #-}
import Control.Monad
import Control.Monad.State
import Control.Monad.Writer

pushWS :: Int ->  WriterT String (State [Int]) ()
pushWS x = WriterT $ state $ \xs -> ((()," push "++ show x),x:xs) 

popWS ::  WriterT String (State [Int]) Int
popWS = WriterT $ state $ \(x:xs) -> ((x," pop "++ show x),xs) 

push :: Int -> StateT [Int] (Writer String) ()
push x = StateT $ \xs -> writer (((),x:xs), " push "++ show x) 

pop :: StateT [Int] (Writer String) Int
pop = StateT $ \(x:xs) -> writer ((x,xs), " pop" ++ show x)

newtype  WS s w a = WS {runWS::s -> (a, s ,w)} deriving Functor
instance Monoid w => Monad (WS s w) where
  return a = WS $ \s -> (a, s, mempty)
  k >>= f = WS $ \s ->
    let (a, s', m) = runWS k s
        (r, ns, m') = runWS (f a) s'
    in (r, ns, m `mappend` m')

instance Monoid w => Applicative (WS s w) where
  pure = return
  (<*>) = ap
