{-# LANGUAGE Arrows #-}

import BouncingBall
import ContinuationFRP
import Control.Arrow

fallingBall :: Ball -> SF a Ball
fallingBall (h0,v0) = proc _ -> do
                           v <- integral v0 -< -g
                           h <- integral h0 -< v
                           returnA -< (h,v)

detectBounce :: SF Ball (Event Ball)
detectBounce = edge detectImpact

inelasticBall :: Ball -> SF a Ball
inelasticBall b = switch (fallingBall b) detectBounce (\_ -> constant (0,0))

elasticBall :: Ball -> SF a Ball
elasticBall b = switch (fallingBall b) detectBounce (\(h,v) -> if abs v < 0.1
                                                                  then constant (0,0)
                                                                  else elasticBall (0, (negate v)*0.6))

-- reactimate (return ()) (return 0.001) (\(h,v) -> h == 0) print (elasticBall (10,0))
