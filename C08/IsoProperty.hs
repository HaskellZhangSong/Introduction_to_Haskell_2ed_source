
f :: (a,b) -> c
f = undefined

g :: a' -> a
g = undefined

(><) :: (a -> b) -> (c -> d) -> (a,c) -> (b,d)
(><) f g (a,b) = (f a, g b)

l = curry f . g
r = curry (f. (g >< id))