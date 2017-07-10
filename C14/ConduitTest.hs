import qualified Data.Conduit.List as CL
import Data.Conduit

l :: Monad m => Source m Int
l = CL.sourceList [1.. 10]

i :: Monad m => Source m Int
i = CL.sourceList [1.. ]

alternative :: Monad m => Conduit a m a
alternative = do 
            p <- await
            case p of
                 Just a -> do 
                      CL.drop 1
                      yield a
                      alternative
                 Nothing -> return ()

sink1 :: Sink Int IO ()
sink1 = CL.mapM_ print
          
