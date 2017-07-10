{-# LANGUAGE DeriveGeneric, DeriveAnyClass, RankNTypes #-}
import Text.PrettyPrint.GenericPretty
import GHC.Generics
import Data.Typeable
import Unsafe.Coerce
import Control.Applicative
import Data.Char

--- type definition
data Company = C {departments :: [Department]}
                   deriving (Show, Typeable, Generic, Out)
data Department = D {departmentName :: String,
                     manager :: Person,
                     workers :: [Person] }
                   deriving (Show, Typeable, Generic, Out)
data Person = P {personName :: Name,
                 gender :: Gender,
                 age    :: Age }
                   deriving (Show, Typeable, Generic, Out)
data Name = N {familyName :: String,
               givenName  :: String }
                   deriving (Show, Typeable, Generic, Out)
data Gender = Male | Female deriving (Show, Typeable, Generic, Out)
type Age = Int

--- Value definition
microsoft_research_cambridge = C [research_haskell,research_fsharp]

research_haskell = D "Haskell Group" simon_peyton_jones
                            [simon_marlow, ralf_lammel]

research_fsharp = D "F# Group" don_syme [andrew_kennedy]

simon_peyton_jones = P (N "Peyton Jones" "Simon") Male 57

simon_marlow = P (N "Marlow" "Simon") Male 41

ralf_lammel = P (N "Ralf" "Lammel") Male 40

don_syme = P (N "Don" "Syme") Male 37

andrew_kennedy = P (N "Andrew" "Kennedy") Male 35

--- Boilerplate code
addAgeP :: Int -> Person -> Person
addAgeP n p = p {age=age p + n}

addAgeD :: Int -> Department -> Department
addAgeD n d@(D dn m ws) = d {manager = addAgeP n m ,
                             workers = map (addAgeP n) ws}

addAgeC :: Int -> Company -> Company
addAgeC n c@(C ds) = c{departments = map (addAgeD n) ds}

mkT :: (Typeable a , Typeable b) => (a -> a) -> (b -> b)
mkT f = case cast f of
            Just f -> f
            Nothing -> id

--- SYB
addAge n = mkT (addAgeP n)

newtype ID x = ID {unID :: x}
newtype Q r a = Q {unQ :: [r] -> [r]}

class Typeable a => Data a where
    gmapT :: (forall b. Data b => b -> b) -> a -> a
    gmapT f x = unID (gfoldl k ID x)
               where 
                  k :: Data d => ID (d -> b) -> d -> ID b
                  k (ID c) x = ID (c (f x))

    gmapQ :: (forall b. Data b => b -> r) -> a -> [r]
    gmapQ f x = unQ (gfoldl k (const (Q id)) x) []
                where
                  -- k :: Data a => Q r a -> a -> Q r a
                  k (Q c) x = Q (\rs -> c (f x : rs))

    gmapM :: Monad m => (forall d. Data d => d -> m d) -> a -> m a
    gmapM f = gfoldl k return
                     where
                        k c x = do c' <- c
                                   x' <- f x
                                   return (c' x')
    gfoldl  :: (forall d b. Data d => c (d -> b) -> d -> c b)
            -> (forall g. g -> c g)
            -> a
            -> c a
    gfoldl _ z = z
    

instance Data Char where
         gmapT f x = x
         gmapQ f x = []
         gmapM f x = return x
                
instance Data Bool where
         gmapT f x = x
         gmapQ f x = []
         gmapM f x = return x
instance Data Int where
         gmapT f x = x
         gmapQ f x = []
         gmapM f x = return x

instance Data Gender where
         gmapT f x = x
         gmapQ f x = []
         gmapM f x = return x
         
instance Data a => Data [a] where
         gmapT f [] = []
         gmapT f (x:xs) = f x : f xs
         gmapQ f [] = []
         gmapQ f (x:xs) = [f x , f xs]
         gmapM f [] = return []
         gmapM f (x:xs) = do
                      x' <- f x
                      xs' <- f xs
                      return (x':xs')

instance Data Name where
         gmapT f (N fn gn) = N (f fn) (f gn)
         gmapQ f (N fn gn) = [f fn, f gn]
         gmapM f (N fn gn) = do
                      fn' <- f fn
                      gn' <- f gn
                      return (N fn' gn')
         gfoldl k z (N fn gn) = ((z N) `k` fn) `k` gn

instance Data Person where
         gmapT f (P name gender age) = P (f name) (f gender) (f age)
         gmapQ f (P name gender age) = [f name, f gender, f age]
         gmapM f (P name gender age) = do
                              name' <- f name
                              gender' <- f gender
                              age' <- f age
                              return (P name' gender' age')
                              
instance Data Department where
         gmapT f (D dn dm ds) = D (f dn) (f dm) (f ds)
         gmapQ f (D dn dm ds) = [f dn, f dm, f ds]
         gmapM f (D dn dm ds) = do
                           dn' <- f dn
                           dm' <- f dm
                           ds' <- f ds
                           return (D dn' dm' ds')

instance Data Company where
         gmapT f (C ds) = C (f ds)
         gmapQ f (C ds) = [f ds]
         gmapM f (C ds) = do
                     ds' <- f ds
                     return (C ds')                     
           
everywhere :: Data a => (forall b. Data b => b -> b) -> a -> a
everywhere f x = f (gmapT (everywhere f) x)

mkQ :: (Typeable a , Typeable b) => r -> (b -> r) -> a -> r
mkQ r q a = case cast a of
                Just b -> q b
                Nothing -> r

everything :: Data a => (r -> r -> r) -> (forall b. Data b => b -> r) -> a -> r
everything k f x = foldl k (f x) (gmapQ (everything k f) x )

getFamilyName :: Person -> [String]
getFamilyName = return.  familyName . personName

everywhereM :: (Monad m, Data a) => (forall b.Data b => b -> m b) -> a -> m a
everywhereM f x = do
                 x' <- gmapM (everywhereM f) x
                 f x'

mkM :: (Typeable a, Typeable b, Typeable (m a), Typeable (m b), Monad m)
       => (a -> m a) -> (b -> m b)
mkM f = case cast f of
            Just  g -> g
            Nothing -> return

addAgeIO :: Person -> IO Person
addAgeIO p = do
        putStr $ show (personName p) ++ ":"
        l <- getLine
        let a = read l :: Int
        return $ p{age=age p + a}

addAgeM :: Data a => a -> IO a
addAgeM = mkM addAgeIO

type GenericQ r = forall a. Data a => a -> r
type GenericT   = forall a. Data a => a -> a
type GenericM m = forall a. Data a => a -> m a

isMarlow :: GenericQ Bool
isMarlow = mkQ False isMarlowP

isMarlowP :: Person -> Bool
isMarlowP p@(P name _ _) = familyName name == "Marlow"

everywhereBut :: GenericQ Bool -> GenericT -> GenericT
everywhereBut q f x | q x = x
                    | otherwise = f (gmapT (everywhereBut q f) x)

extQ :: (Typeable a, Typeable b) => (a -> r) -> (b -> r) -> (a -> r)
extQ q f a = case cast a of
                 Nothing -> q a
                 Just b  -> f b

type HcInfo = ([(String,Int)],Int)

hcP :: Person -> [HcInfo] -> HcInfo
hcP p _ = ([],1)

addResults :: [HcInfo] -> HcInfo
addResults rs = (concat (map fst rs) , sum (map snd rs))
           
hcD :: Department -> [HcInfo] -> HcInfo
hcD (D d m ws) subs = ((d,n):l,n)
        where
            (l,n) = addResults subs

hcG :: Data a => a -> [HcInfo] -> HcInfo
hcG node subs = addResults subs

queryUp :: (forall a. Data a => a -> [r] -> r) -> GenericQ r
queryUp f x = f x (gmapQ (queryUp f) x)

hc :: Company -> [(String, Int)]
hc = fst . queryUp (hcG `extQ` hcD `extQ` hcP)

extT :: (Typeable a, Typeable b) => (a -> a) -> (b -> b) -> a -> a
extT def ext s = case cast ext of
                   Just f -> f s
                   Nothing -> def s

capName :: Name -> Name
capName (N f n) = N f (map toUpper n)

depName :: Department -> Department
depName (D n m sub) = D (map toUpper n) m sub

capComp :: Data a => a -> a
capComp = everywhere (id `extT` depName `extT` capName)

ext0 :: (Typeable a, Typeable b) => c a -> c b -> c a
ext0 def ext = maybe def id (gcast ext)

newtype M m x = M {unM :: x -> m x}

extM :: (Monad m, Typeable a, Typeable b) => (a -> m a) -> (b -> m b) -> a -> m a
extM def ext = unM (ext0 (M def) (M ext))
