{-# LANGUAGE Arrows #-}

import FRP.Yampa

-- test0 = reactimate (return 100.0)
--                    (\_ -> return (1.0, Just 5))
--                    (\_ b -> (putStrLn $ show b) >> return False)
--                    (arr (+1))

-- test1 = reactimate (return (100.0))
--                    (\_ -> return (1.0, Just 5))
--                    (\c b -> (putStrLn $ show b) >> if b == 6.0 then return True else return False)
--                    (arr (+1))

type Pos = Double
type Vel = Double

type Ball = (Pos, Vel)

g :: Double
g = 9.81

fallingBall :: Ball -> SF () (Pos,Vel)
fallingBall (y0 ,v0) = proc () -> do
               v <- (v0 +) ^<< integral -< (-g)
               y <- (y0 +) ^<< integral -< v
               returnA -< (y,v)

detectBounce :: SF (Double, t) (Event (Double, t))
detectBounce = when (\(pos , v) -> pos <= 0)

elasticBall :: Ball -> SF () Ball
elasticBall (p, v) = rswitchWhen (fallingBall (p, v)) 
                                 detectBounce (fallingBall . \(p',v') -> (p',negate v'))

when :: (a -> Bool) -> SF a (Event a)
when p = proc a -> do
       event <- edge -< p a
       returnA -<  (event `tag` a)


rswitchWhen :: SF a b -> SF b (Event c) -> (c -> SF a b) -> SF a b
rswitchWhen sf sfe f = rswitch (sf >>> forkSecond sfe) 
                               (\e -> f e >>> forkSecond sfe)

rswitch :: SF a (b, Event c) -> (c -> SF a (b, Event c)) -> SF a b
rswitch sf f = switch sf (\e -> rswitch (f e >>> sfSecond notYet) f)

forkSecond :: SF a c' -> SF a (a, c')
forkSecond sf = sfFork >>> sfSecond sf

sfSecond sf = identity *** sf

sfFork = arr fork

fork a = (a,a)
