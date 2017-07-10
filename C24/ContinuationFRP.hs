{-# LANGUAGE DeriveFunctor #-}

module ContinuationFRP where
import Control.Arrow
import Control.Category
import Prelude hiding ((.) , id)

type Time  = Double
type DTime = Double

data SF a b = MkSF {runSF :: a -> DTime -> (b,SF a b)}

runSFs :: [SF a b] -> a -> DTime -> [(b,SF a b)]
runSFs sfs a dt = map f sfs
  where
    -- f :: SF a b -> (b,SF a b)
    f sf = runSF sf a dt

reactimate :: IO a -> IO DTime -> (b -> Bool) -> (b -> IO ()) -> SF a b -> IO ()
reactimate getInput getTick terminate putOutput sf0 = loop sf0
  where
    -- loop :: SF a b -> IO ()
    loop sf = do a  <- getInput
                 dt <- getTick
                 let (b,sf') = runSF sf a dt
                 putOutput b
                 if terminate b 
                   then return ()
                   else loop sf'

integral :: Double -> SF Double Double
integral v0 = MkSF (\a dt -> let v = a * dt + v0
                             in (v, integral v))

instance Category SF where
    id = MkSF $ \a -> \t -> (a, id)
    sf2 . sf1 = MkSF (\a dt -> let (b,sf1') = runSF sf1 a dt
                                   (c,sf2') = runSF sf2 b dt
                               in (c,sf2' . sf1'))

instance Arrow SF where
    arr f = MkSF (\a _ -> let b = f a
                          in (b,arr f))
    -- f :: SF b c = b -> DTime -> (c , SF b c)
    -- res :: (b,d) -> DTime -> ((c,d), SF (b,d) (c,d))
    first f = MkSF $ \(b,d) -> \dt -> let (c,sf1) = runSF f b dt
                                      in ((c,d), first sf1)

instance ArrowChoice SF where
    -- f :: SF b c = b -> DTime -> (c, SF b c)
    -- res :: (Either b d) -> DTime -> (Either c d, SF (Either b d) (Either c d))
    left sf@(MkSF f) = MkSF $ \r -> \dt -> case r of 
                                             Left a -> let (r,sf1) = f a dt in (Left r, left sf1)
                                             Right b -> (Right b, left sf)

instance ArrowLoop SF where
  loop sf@(MkSF f) = MkSF $ \b -> \dt ->
                   let (~(c,d), sf') = f (b,d) dt in (c, loop sf')

sinSF :: SF Double Double
sinSF = MkSF (\d dt -> (sin (d + dt), (\x -> x + dt) ^>> sinSF))

data Event v = NoEvent | AnEvent v
                       deriving (Show, Functor)

edge :: (a -> Bool) -> SF a (Event a)
edge p = edgeAux True
  where
    -- edgeAux :: Bool -> SF a Event
    edgeAux b = MkSF (\ a _ -> let q  = p a
                                   e  = if q && (not b) then AnEvent a else NoEvent
                                   sf = if q then edgeAux True else edgeAux False
                                in (e,sf))

edgeTest = sinSF >>> edge (> 0) 

parB :: [SF a b] -> SF a [b]
parB sfs = MkSF (\ a dt -> let bsfs      = runSFs sfs a dt
                               (bs,sfs') = unzip bsfs
                            in (bs, parB sfs'))

constant :: b -> SF a b
constant b = MkSF (\ _ _ -> (b,constant b))

localTime :: SF a Time
localTime = constant 1 >>> integral 0

after :: Time -> SF a (Event Time)
after t = localTime >>> edge (>= t)

switch :: SF a b -> SF b (Event v) -> (v -> SF a b) -> SF a b
switch sf1 sfe f   = MkSF (\ a dt -> let (b, sf1')  = runSF sf1 a dt
                                         (ev, sfe') = runSF sfe b dt
                                         sf'        = case ev of
                                                        NoEvent   -> switch sf1' sfe' f
                                                        AnEvent v -> f v
                                      in (b,sf'))

switch' :: SF () b -> (v -> b -> SF (Event v) b) -> SF (Event v) b
switch' sf1 f = MkSF (\ ev dt -> let (b,sf1') = runSF sf1 () dt
                                     sf'      = case ev of
                                                  NoEvent   -> switch' sf1' f
                                                  AnEvent v -> f v b
                                  in (b,sf'))

pSwitch :: [SF a b] -> SF (a,Event (SF a b)) [b]
pSwitch sfs =  MkSF (\ (a,ev) dt -> let newSFs = case ev of
                                                   NoEvent       -> sfs
                                                   AnEvent newSF -> newSF : sfs
                                        bsfs = runSFs newSFs a dt
                                        (bs,sfs') = unzip bsfs
                                     in (bs, pSwitch sfs'))

