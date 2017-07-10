-- derive.hs
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -ddump-splices #-}

import Data.Derive.Class.Arities
import Data.Derive.Arities
import Data.Derive.Show
import Data.Derive.Eq
import Data.DeriveTH

data Shape = Circle Double | Triangle Double Double Double

derive makeEq ''Shape
derive makeShow ''Shape
derive makeArities ''Shape