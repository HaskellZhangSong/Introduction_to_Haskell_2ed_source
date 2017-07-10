import Data.List (insertBy, sortBy)
import Data.Ord (comparing) 

data HTree a = Leaf a | Branch (HTree a) (HTree a) deriving Show

htree [(_, t)] = t 
htree ((w1,t1):(w2,t2):wts) = htree $ insertBy 
                                        (comparing fst) 
                                        (w1 + w2, Branch t1 t2) wts

serialize (Leaf x) = [(x, "")]
serialize (Branch l r) = [(x,'0':code)|(x,code)<-serialize l]++[(x,'1':code)| (x,code) <-serialize r] 

huffman :: (Ord a, Ord w, Num w) => [(a,w)] -> [(a,[Char])] 
huffman freq = sortBy (comparing fst) $ serialize 
      $ htree $ sortBy (comparing fst) $ [(w, Leaf x) | (x,w) <- freq] 