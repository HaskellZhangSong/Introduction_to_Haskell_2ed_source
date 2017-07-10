{-# LANGUAGE FlexibleContexts,UndecidableInstances #-}


import Control.Arrow
import qualified Control.Category as Cat
import Data.List (union)

data StaticParser s = SP Bool [s]
newtype DynamicParser s a b = DP {runDP :: (a, [s]) -> (b, [s])}
data Parser s a b = P {static :: (StaticParser s)
                      ,dynamic :: (DynamicParser s a b)}

spCharA :: Char -> StaticParser Char
spCharA c = SP False [c]

dpCharA :: Char -> DynamicParser Char Char Char
dpCharA c = DP (\(_,x:xs) -> (x,xs))

charA :: Char -> Parser Char Char Char
charA c = P (SP False [c]) (DP (\(_,x:xs) -> (x,xs)))

instance Eq s => Cat.Category (Parser s) where
    id = P (SP True []) (DP (\(b,s) -> (b,s)))
    (P (SP empty1 start1) (DP p2)) .
          (P (SP empty2 start2) (DP p1)) =
            P (SP (empty1 && empty2)
                  (if not empty1 then start1 else start1 `union` start2))
              (DP (p2.p1))

instance (Cat.Category (Parser s) , Eq s) => Arrow (Parser s) where
   arr f = P (SP True []) (DP (\(b,s) -> (f b,s)))
   first (P sp (DP p)) = P sp (DP (\((b,d),s) ->
        let (c, s') = p (b,s)
        in  ((c,d),s')))

