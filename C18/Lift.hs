{-# LANGUAGE TemplateHaskell #-}
import Language.Haskell.TH.Syntax
import Control.Applicative

data Option a = None | Some a deriving (Show, Eq)

instance Lift a => Lift (Option a) where
  lift None = return (ConE 'None)
  lift (Some a) = liftA (ConE 'Some `AppE`) (lift a)
