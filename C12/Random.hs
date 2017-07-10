import System.Random

rollDice :: Int -> IO ()
rollDice n = do
        gen <- newStdGen
        print $ take n (randomRs ((1,6)::(Int,Int)) gen)