import Data.Monoid
import Control.Monad.Reader

type Stack = [Int]
(|>) :: Monoid a => a -> a -> a
(|>) = mappend

newtype FunApp a = FunApp { appFunApp :: a -> a }
instance Monoid (FunApp a) where
  mempty = FunApp id
  FunApp f `mappend` FunApp g = FunApp (g . f)

push :: Int -> FunApp Stack
push i = FunApp $ \xs -> i:xs

pop :: FunApp Stack
pop = FunApp $ \(x:xs) -> xs

m :: FunApp Stack
m =    push 3
    |> push 1
    |> pop
    
readLength :: Int -> Reader Stack Int
readLength n = reader $ \xs -> length xs