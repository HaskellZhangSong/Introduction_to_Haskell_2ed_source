{-# LANGUAGE DeriveAnyClass #-}

class MyShow a where
  myshow :: a -> String
  myshow _ = "default"
  
data Person = P String Int
instance MyShow Person
