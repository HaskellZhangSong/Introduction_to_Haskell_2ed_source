{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses,FlexibleContexts #-}

class I a b where
      ifoo :: a -> b -> String

instance {-# OVERLAPPABLE #-} I a [b] where
      ifoo _ _ = "I a [b]"

instance {-# INCOHERENT  #-} I a [a] where
      ifoo _ _ = "I a [a]"

instance {-# OVERLAPPABLE #-} I Int [a] where
      ifoo _ _ = "I Int [a]"

foo :: I a [b] => a -> [b] -> String
foo a b = ifoo a b
