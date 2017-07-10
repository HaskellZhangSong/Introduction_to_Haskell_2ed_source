-- ExprQuoteTest.hs
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -ddump-splices #-}
import ExprQuote

foo [expr|x+1|]= [expr|1+1|]
foo _ = Val 10

 
bar [expr|x+0|] = x
bar [expr|0+x|] = x
bar x = x