{-# LANGUAGE KindSignatures, TypeSynonymInstances, FlexibleInstances, TypeOperators,  UndecidableInstances, DefaultSignatures,FlexibleContexts,DeriveGeneric,DeriveAnyClass #-}

import GHC.Generics
import Data.Typeable

class GShow (a :: * -> *) where
  shows1 :: Bool -> a x -> ShowS

instance GShow V1 where
  shows1 _ _ = error "cannot shows1 Void type"

instance GShow U1 where
  shows1 _ U1 = id

instance (GShow a, GShow b) => GShow (a :+: b) where
  shows1 b (L1 a) = shows1 b a
  shows1 b (R1 a) = shows1 b a

instance (GShow a, GShow b) => GShow (a :*: b) where
  shows1 b (x :*: y) = shows1 b x . shows1 b y

instance (Show0 a) => GShow (K1 i a) where
  shows1 _ (K1 a) = \x -> show0 a ++ x

instance (GShow a) => (GShow (D1 b a)) where
  shows1 b (M1 a) = shows1 b a

instance (GShow a, Constructor g) => GShow (C1 g a) where
  shows1 _ c@(M1 a) =  showString "(".
                       showString (conName c) .
                       showString " " .
                       wrapRecord (shows1 (conIsRecord c) a) .
                       showString ")"
                      where
                        wrapRecord :: ShowS -> ShowS
                        wrapRecord s | conIsRecord c = showString "{ " . s . showString " }"
                                     | otherwise = s

instance (GShow a, Selector g) => GShow (S1 g a) where
  shows1 b s@(M1 a) | null (selName s) = shows1 b a
                    | otherwise = showString (selName s)
                                  . showString " = " . shows1 b a . showChar ' '

shows_default :: (Generic a, GShow (Rep a)) => a -> ShowS
shows_default x = shows1 False (from x)

class Show0 a where
  show0 :: a -> String
  default show0 :: (Generic a, GShow (Rep a)) => a -> String
  show0 x = shows_default x ""
  
instance Show0 Char where
  show0 a = show a
instance Show0 Int where
  show0 a = show a
instance Show0 Bool where
  show0 a = show a
instance Show a => Show0 [a] where
  show0 a = show a

data Person = Person {name :: String, age :: Int} deriving (Eq, Generic,Show0)

data Person' = P String Int deriving (Eq,Generic,Show0)

data Nat = Zero | Succ Nat deriving (Eq, Generic,Show0)

data Sum a b = L a | R b deriving (Eq, Generic,Show0)
