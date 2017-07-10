{-# LANGUAGE DeriveGeneric,DefaultSignatures,KindSignatures,FlexibleContexts #-}
import GHC.Generics

class GFunctor (f :: * -> *) where
      gfmap :: (a -> b) -> (f a -> f b)
      default gfmap :: (Generic1 f, GFunctor (Rep1 f)) => (a -> b) -> (f a -> f b)
      gfmap = defaultfmap

defaultfmap = undefined --
