main::IO ()
main = do
        putStr "what is your name?"
        name <- getLine
        putStrLn $ "Hello " ++ name