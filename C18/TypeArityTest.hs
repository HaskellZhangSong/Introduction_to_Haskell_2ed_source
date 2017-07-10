{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -ddump-splices #-}
import TypeArity
import Data.Proxy

makeTypeArity ''Int
makeTypeArity ''Either
makeTypeArity ''(,)