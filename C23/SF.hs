{-# LANGUAGE Arrows,DeriveFunctor #-}
import Control.Arrow
import Control.Category
import Prelude hiding (id,(.))
import qualified Prelude as Prelude


data SF a b = SF {runSF :: [a] -> [b]}

instance Category SF where
   id = SF (Prelude.id)
   (.) (SF f) (SF g) = SF ((Prelude..) f g)

instance Arrow SF where
   arr f = SF (map f)
   -- f  :: [b] -> [c] 
   -- bd :: [(b,d)]
   -- res :: [(c,d)]
   first (SF f) = SF $ \bd -> let (bs,ds) = unzip bd 
                                  in zip (f bs) ds

class ArrowLoop a => ArrowCircuit a where
   delay :: b -> a b b

instance ArrowCircuit SF where
   delay x = SF (init. (x:))

mapA' :: SF b c -> SF [b] [c]
mapA' (SF f) = SF (\xs -> map f xs)

instance ArrowChoice SF where
         left (SF f) = SF (\xs -> combine xs (f [y | Left y <- xs]))
              where combine (Left y:xs) (z:zs) = Left z: combine xs zs
                    combine (Right y:xs) zs = Right y: combine xs zs
                    combine [] zs = []

listcase [] = Left ()
listcase (x:xs) = Right (x,xs)

mapA :: ArrowChoice a => a b c -> a [b] [c]
mapA f = arr listcase >>>
         arr (const []) ||| (f *** mapA f >>> arr (uncurry (:)))

mapA'' ::  ArrowChoice a => a b c -> a [b] [c]
mapA'' f = proc xs -> do
               case xs of 
                    [] -> returnA -< []
                    (x:xs') -> do
                            y <- f -< x
                            ys <- mapA f -< xs'
                            returnA -< y:ys

instance ArrowLoop SF where
    -- f  :: [(b,d)] -> [(c,d)]
    -- as :: [b] 
    loop (SF f) = SF $ \as -> 
                let (bs,cs) = unzip (f (zip as (stream cs))) in bs
         where stream ~(x:xs) = x:stream xs

showBools :: [Bool] -> String
showBools [] = ""
showBools (False : xs) = '_' : showBools xs
showBools (True  : xs) = '^' : showBools xs

toBools :: String -> [Bool]
toBools "" = []
toBools ('_': xs) = False : toBools xs
toBools ('^': xs) = True: toBools xs
toBools _ = error "only ^ and _ can be used."

s1 ,s2:: [Bool]
s1 = toBools "____^^^^^____^^^^^____"
s2 = toBools "_^_^_^_^_^_^_^_^_^_^_^"
s3 = toBools "^^^^^^^^^^^^^^^^^^^^^^"
s4 = toBools "______________________"
s5 = toBools "__________^___________"

edge :: SF Bool Bool
edge = arr id &&& delay False >>> arr detect
       where detect (a,b) = a && not b

edge' :: SF Bool Bool
edge' = proc b -> do 
           c <- delay False -< b
           returnA -< b && not c

nor :: SF (Bool,Bool) Bool
nor = arr (not.uncurry (||))

counter :: ArrowCircuit a => a Bool Int
counter = proc reset -> do
                     rec output <- returnA -< if reset then 0 else next
                         next   <- delay 1 -< output + 1
                     returnA -< output

counter' :: ArrowCircuit a => a Bool Int
counter' = loop (arr (\(reset, next) -> 
                        dup (if reset then 0 else next)) >>>
                     (first (arr (+1) >>> delay 1) >>> aswap))

dup :: Arrow arrow => arrow a (a,a)
dup = arr $ \a -> (a,a)

aswap :: Arrow a => a (b,c) (c,b)
aswap = arr $ \(a,b) -> (b,a)

flipflop :: SF (Bool, Bool) (Bool, Bool)
flipflop = loop (arr (\((reset,set),(c,d)) -> ((set,d),(reset,c))) >>>
                 nor *** nor >>>
                 delay (False,True) >>>  -- reset
                 arr id &&& arr id)

flipflop' :: SF (Bool, Bool) (Bool, Bool)
flipflop' = proc (reset,set) -> do
                 rec c <- delay False -< nor' reset d
                     d <- delay True -< nor' set c
                 returnA -< (c,d)
          where nor' a b = not (a || b)

-- bimap showBools showBools  (unzip (runSF flipflop' (zip s1 s2)))
-- let (r1,r2) = bimap showBools showBools (unzip (runSF flipflop' (zip s1 s2))) in putStrLn r1 >> putStrLn r2

parB :: Arrow arr => [arr a b] -> arr a [b]
parB [] = arr $ \_ -> []
parB (x:xs) = proc a -> do
                  b <- x -< a
                  bs <- parB xs -< a
                  returnA -< b:bs
