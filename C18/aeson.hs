{-# LANGUAGE DeriveGeneric,TemplateHaskell,OverloadedStrings #-}

import GHC.Generics
import Data.Aeson
import Data.Char
import Data.Aeson.TH

data Recipe = Recipe
      { reciName :: String
      , reciIngredients :: [Ingredient]
      } deriving (Show, Eq, Generic)

data Ingredient = Ingredient 
      { ingrName :: String
      , ingrQuantity :: Int
      , ingrMeasure :: Maybe String
      } deriving (Show, Eq,Generic)

(deriveJSON defaultOptions{fieldLabelModifier = 
                            (map toLower).(drop 4)} ''Recipe)

(deriveJSON defaultOptions{fieldLabelModifier = 
                            (map toLower).(drop 4)} ''Ingredient)

cake :: Ingredient
cake = Ingredient { ingrName = "Ciambellone Cake",
                    ingrQuantity = 250,
                    ingrMeasure = Just "gram"}
