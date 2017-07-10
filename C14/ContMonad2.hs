import Control.Monad.Cont

fib_cps :: Int -> ContT r IO Int
fib_cps 0 = return 1
fib_cps 1 = return 1
fib_cps n = do
        n2 <- fib_cps (n-2)
        liftIO $ putStrLn $ "fib_cps " ++ show (n-2) ++ "=" ++ show n2
        n1 <- fib_cps (n-1)
        liftIO $ putStrLn $ "fib_cps " ++ show (n-1) ++ "=" ++ show n1
        return (n1 + n2)
