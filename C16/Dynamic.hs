import Data.Dynamic
import Control.Applicative 

matchZero :: Dynamic -> Maybe Int
matchZero d = case fromDynamic d :: Maybe Int of
                   Nothing -> Nothing
                   Just c -> if c == 0 then return 0 else Nothing

matchBool :: Dynamic -> Maybe Int
matchBool d = case fromDynamic d :: Maybe Bool of
                   Nothing -> Nothing
                   Just c -> if c then return 1 else return 0

dynamicMatch :: Dynamic -> Maybe Int
dynamicMatch a = foldl (<|>) Nothing [matchZero a, matchBool a]
