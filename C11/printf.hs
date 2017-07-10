{-# LANGUAGE FlexibleInstances, OverlappingInstances #-}

class Printf t where
       printf :: String -> t

instance Printf (IO ()) where
        printf cs = putStrLn cs

format :: Show t => String -> t -> String
format ('%' : 's' : cs) cs' = show cs' ++ cs
format (c : cs) cs' = c : format cs cs'
format "" cs' = ""

instance Show t => Printf (t -> IO ()) where
        printf cs x = putStrLn (format cs x)

instance (Show u, Printf t) => Printf (u -> t) where
     printf cs = \x -> printf (format cs x)	
     
test1 :: IO ()
test1 = printf "%s and %s are friends." "Mike" "Jane"

test2 :: IO ()
test2 = printf "%s, %s and %s are friends." "Mike" "Jane" "Chris"