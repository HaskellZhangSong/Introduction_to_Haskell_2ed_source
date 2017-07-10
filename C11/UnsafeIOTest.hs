import System.IO.Unsafe
import Data.IORef

ref :: IORef Int
ref = unsafePerformIO $ newIORef 0

plus :: IO ()
plus = do
     x <- readIORef ref
     y <- writeIORef ref 1 >> return 100
     print (x + y)

plus' :: IO ()
plus' = do
     x <- unsafeInterleaveIO $ readIORef ref 
     y <- unsafeInterleaveIO $ writeIORef ref 1 >> return 100
     print (x + y)

plus'' :: IO ()
plus'' = do
     x <- unsafeInterleaveIO $ readIORef ref
     y <- unsafeInterleaveIO $ writeIORef ref 1 >> return 100
     print (y + x)
