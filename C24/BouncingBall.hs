module BouncingBall where

type Acceleration = Double
type Velocity = Double
type Height = Double

type Ball = (Height, Velocity)

g :: Acceleration
g = 9.81

detectImpact :: Ball -> Bool
detectImpact (h , v) = h <= 0

negateVel :: Ball -> Ball
negateVel (h , v) = (h , negate v)

detectBounce :: SF (Double, t) (Event (Double, t))
detectBounce = when (\(pos , v) -> pos <= 0)

elasticBall :: Ball -> SF () Ball
elasticBall (p, v) = rswitchWhen (fallingBall (p, v)) 
                                 detectBounce (fallingBall . \(p',v') -> (p',negate v'))

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
