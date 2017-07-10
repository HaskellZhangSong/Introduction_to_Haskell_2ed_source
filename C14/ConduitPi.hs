import Data.Conduit
import qualified Data.Conduit.List as CL
import System.Random
import Control.Monad.IO.Class
import Control.Monad
import Control.Monad.Trans
randomCL :: MonadIO m => Producer m Double
randomCL =  forever $ do
           a <- liftIO (randomIO :: IO Double)
           yield a

square :: Monad m => Conduit Double m Int
square = forever $ do
       mx <- await
       my <- await
       case ((\x y -> (x-0.5) ^ 2 + (y-0.5) ^2) <$> mx <*> my) of
            Just a -> yield $ if (a <= 0.25) then 1 else 0
            Nothing -> return ()

times = 1500 :: Int

cpi :: IO Int
cpi = runConduit $ randomCL $= square $= CL.isolate times $= CL.fold (+) 0
     
main :: IO ()
main = do
     as <- cpi
     print $ fromIntegral as / fromIntegral times * 4
