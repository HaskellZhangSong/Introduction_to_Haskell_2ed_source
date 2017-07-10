{-# LANGUAGE MultiParamTypeClasses,FlexibleInstances #-}

class GEq a b where
    geq :: a -> b -> Bool

instance {-# OVERLAPPABLE #-}Real b => GEq Double b where
   geq a b = toRational a == toRational b
  
instance {-# OVERLAPPABLE #-} Real a => GEq a Double where
   geq a b = toRational a == toRational b

instance {-# OVERLAPPING #-} GEq Double Double where
   geq a b = toRational a == toRational b
