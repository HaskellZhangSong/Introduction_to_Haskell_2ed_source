{-# LANGUAGE FlexibleContexts #-}

import Control.Applicative

instance Num b => Num (a -> b) where
        (+) = liftA2 (+)
        (-) = liftA2 (-)
        (*) = liftA2 (*)
        abs = liftA abs
        signum = liftA abs
        negate = fmap negate
        fromInteger = pure.fromInteger

instance Integral a => Integral (a -> b) where