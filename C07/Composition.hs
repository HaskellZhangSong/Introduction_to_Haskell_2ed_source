f,g,h :: Num a => a -> a
f x = 4*x+1
g x = x^2+1
h x = f (g x)

infix 9 >>
(>>) :: (a -> b) -> (b -> c) -> (a -> c)
(>>) = flip (.)

(|>) :: b -> (b -> c) -> c
(|>) = flip ($)