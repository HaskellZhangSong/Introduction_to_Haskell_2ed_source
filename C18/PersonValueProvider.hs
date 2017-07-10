
{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass, DeriveDataTypeable,QuasiQuotes #-}
module PersonValueProvider where
import Data.Aeson
import GHC.Generics
import Data.String
import Data.Maybe
import Data.Data
import Language.Haskell.TH.Quote
import Language.Haskell.TH

data Person = Person {name :: String , age :: Int}
                deriving (Show, Generic, FromJSON, Data)

quoteJSONPerson :: String -> ExpQ
quoteJSONPerson p = dataToExpQ (const Nothing) 
                               ((fromJust.decode.fromString) p :: Person)

personJSON :: QuasiQuoter
personJSON = QuasiQuoter { quoteExp = quoteJSONPerson }

personJSON_file = quoteFile personJSON
