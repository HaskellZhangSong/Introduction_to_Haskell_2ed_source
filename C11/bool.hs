import Data.IORef

bool :: IO ()
bool = do
     bRef <- newIORef True
     modifyIORef bRef not
     b <- readIORef bRef
     print b
     writeIORef bRef True
     b <- readIORef bRef
     print b