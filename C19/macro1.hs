{-# LANGUAGE CPP,DeriveGeneric,DeriveDataTypeable #-}
import GHC.Generics
import Data.Typeable
import Data.Data
import Data.Aeson
import Text.PrettyPrint.GenericPretty

#define DERIVING deriving (Show, Generic, Data, Typeable)
data Company = C {departments :: [Department]} DERIVING
data Department = D {departmentName :: String,
                     manager :: Person,
                     workers :: [Person] } DERIVING
data Person = P {personName :: Name,
                 gender :: Gender,
                 age    :: Age } DERIVING

data Name = N {familyName :: String,
               givenName  :: String } DERIVING

data Gender = Male | Female DERIVING

type Age = Int

#define MAX(a,b) (if (a < b) \
              then (b) \
              else (a))

foo = MAX(10,20)

instance FromJSON Gender
instance ToJSON Gender
instance Out Gender

#define EMPTY_INSTANCES(T) instance FromJSON (T); instance ToJSON (T); instance Out (T) 

EMPTY_INSTANCES(Name)
EMPTY_INSTANCES(Person)
EMPTY_INSTANCES(Department)
EMPTY_INSTANCES(Company)


#if defined(PAR)
import Control.Parallel
import Control.Parallel.Strategies
import Control.DeepSeq
#endif

#if defined(PAR)
test = sum ( map expensiveFunc myList `using` strat )
    where strat = parListChunk 100 rseq
#else 
test = sum ( map expensiveFunc myList )
#endif
