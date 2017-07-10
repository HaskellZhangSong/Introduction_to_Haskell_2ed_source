{-# LANGUAGE ScopedTypeVariables #-}
import qualified Data.Vector.Unboxed as U
import System.Random.MWC
import Control.Monad.ST
import Control.DeepSeq
import System.Random

random' :: Int -> IO (U.Vector Double)
random' n = do
  g <- newStdGen
  return $ U.fromList (take n (randoms g))

random'' :: Int -> IO (U.Vector Double)
random'' n = do
  vs <- withSystemRandom $
        \(gen::GenST s) -> uniformVector gen n :: ST s (U.Vector Double)
  return $ U.force (vs :: (U.Vector Double))
  
getPiV :: IO Double
getPiV = do
   xs <- random' 6000
   ys <- random' 6000
   let isIn = U.zipWith (\x y -> if (x-0.5)^2 + (y-0.5)^2 <= 0.25
                                 then 1.0 else 0) xs ys
   return $ (4.0 :: Double) * (force (U.sum isIn)) / (6000 :: Double)

main = do
  ps <- U.forM (U.fromList [1..2000 :: Int]) (\i -> getPiV)
  putStrLn . show $ force $ U.sum ps / (2000 :: Double)
