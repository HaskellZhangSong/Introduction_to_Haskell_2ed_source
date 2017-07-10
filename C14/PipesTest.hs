{-# LANGUAGE RankNTypes #-}
import Pipes
import qualified Pipes.Prelude as P
import Control.Monad

hello :: Pipe String String IO r
hello = forever $ do
                 lift $ putStr "Please tell me your name:"
                 name <- await
                 yield ("Hello " ++ name)

say_hello :: Effect IO ()
say_hello = P.stdinLn >-> hello >-> P.stdoutLn

main1 = runEffect say_hello

await' :: Monad m => Consumer a m a
await' = await 

yield' :: Monad m => a -> Producer' a m ()
yield' = yield

drop1keep1 :: Monad m => Pipe a a m ()
drop1keep1 = P.drop 1 >-> P.take 1

alternatives :: Monad m => Pipe a a m ()
alternatives = replicateM_ 5 drop1keep1

test1 :: [Int]
test1 = P.toList ((each [1..]) >-> alternatives) 

test2 :: IO ()
test2 = runEffect (for (each [1.. ] >-> alternatives) (lift.print))

plus' :: Monad m => Consumer Int m String
plus' = do
     a <- await
     b <- await
     return $ show $ a + b

plus :: Monad m => Pipe Int String m ()
plus = forever $ do
      a <- await
      b <- await
      yield $ show $ a + b

-- runEffect $ (lift readLn >~ plus) >~ P.stdoutLn
