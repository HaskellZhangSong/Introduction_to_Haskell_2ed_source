-- StandaloneDeriveTest.hs
{-# LANGUAGE TemplateHaskell, StandaloneDeriving, DeriveGeneric, DeriveAnyClass #-}     
-- ghcqyl{}使用元编程需要的，之后的ghc{}版本会改。     
{-# LANGUAGE KindSignatures,ConstraintKinds #-}     
{-# OPTIONS_GHC -ddump-splices  #-}     

import DeriveTopdown
import qualified GHC.Generics as G
import qualified Data.Binary as B
import qualified Data.Aeson as A
import qualified Data.Data as D

data C a b = A (B a) G
data B a = B a | F (D a)
data D b = D b | E b
data G = G
derivings [''Eq, ''G.Generic, ''B.Binary, ''Ord] ''C