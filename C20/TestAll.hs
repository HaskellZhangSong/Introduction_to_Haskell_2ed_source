{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -ddump-splices #-}
module TestAll where
import Test.QuickCheck.All
import QuickCheck1 (prop_bar,prop_foo,check)

test = check


