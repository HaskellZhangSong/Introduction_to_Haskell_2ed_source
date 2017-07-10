-- IsoEither.hs
f :: (a, Either b c) -> Either (a,b) (a,c)
f (a, Left b) = Left (a,b)
f (a, Right c) = Right (a,c)

g :: Either (a,b) (a,c) -> (a, Either b c)
g (Left (a, b)) = (a, Left b)
g (Right (a, c)) = (a, Right c)
