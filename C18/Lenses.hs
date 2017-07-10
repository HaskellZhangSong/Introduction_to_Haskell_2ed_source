{-# LANGUAGE TemplateHaskell #-}
import Control.Lens
data Name = N {_firstName  :: String , 
               _familyName :: String } 
				     deriving (Show, Eq)
				     
makeLenses ''Name 

name = N "Song" "Zhang"
