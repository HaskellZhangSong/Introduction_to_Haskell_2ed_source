-- ContMonad3.hs
import Control.Monad.Cont

fibs2 :: Int -> Cont r Int
fibs2 0 = return 1
fibs2 1 = return 1
fibs2 n = do
       n1 <- callCC $ \k -> (fibs2 (n - 1))
       n2 <- callCC $ \k -> (fibs2 (n - 2))
       return (n1 + n2)

print4 :: ContT r IO ()
print4 = do
       (goto, n) <- callCC $ \k -> let f x = k (f, x) in return (f,0)
       if n < 4
          then do
               lift $ putStrLn "Hello" 
               goto (n + 1)
          else return ()


fact_cps2 :: Int -> Cont r Int
fact_cps2 n = do 
           (goto, acc, num) <- callCC $ \k -> let f x y = k (f,x,y)
                                              in return (f, 1, n)
           if num == 1
              then return acc
              else goto (acc * num) (num - 1)
                  
          
fact_cps2' n = (callCC $ \k -> let f x y = k (f,x,y) in return (f,1,n)) >>=
               \(goto,acc,num) -> if num == 1 
                                   then return acc 
                                   else goto (acc* num) (num - 1)


fact_cps2'' n = cont (\h -> runCont (let f = \x y -> (cont $ \_ -> h (f,x,y)) in return (f,1,n)) h) >>=
               \(goto,acc,num) -> if num == 1 
                                   then return acc 
                                   else goto (acc* num) (num - 1)

fact_cps2''' n = \br -> 
                 (\h -> (let f = \x y -> (\_ -> h (f,x,y)) 
                                 in \k -> k (f,1,n)) h)
                 (\a -> ((\(goto,acc,num) -> if num == 1 
                                                     then \k -> k acc 
                                                     else goto (acc* num) (num - 1)) a) (\b -> br b))
