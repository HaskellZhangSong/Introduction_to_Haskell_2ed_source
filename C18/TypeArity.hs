{-# LANGUAGE TemplateHaskell, PolyKinds #-}
module TypeArity where
import Data.Proxy
import Language.Haskell.TH

class TypeArity (cla :: k) where
  arity :: Proxy cla -> Integer

getTypeArity :: Name -> Q Int
getTypeArity name = do
    info <- reify name
    case info of 
        TyConI dec ->
            case dec of
                DataD    _ _ tvbs cons _ -> return $ length tvbs
                NewtypeD _ _ tvbs con  _ -> return $ length tvbs
                _ -> error "The type must be data, newtype definition!"
        _ -> error "bad type name, quoted name is not a type!"

makeTypeArity :: Name -> Q [Dec]
makeTypeArity name = do
    at <- getTypeArity name
    let fName = mkName "arity"
    let fun = [FunD fName [Clause [WildP]
                    (NormalB (LitE (IntegerL (fromIntegral at))))
                    []]]
    return $ [InstanceD [] (AppT (ConT ''TypeArity) (ConT name)) fun]

{-#
makeTypeArity :: Name -> Q [Dec]
makeTypeArity name = do
    at <- getTypeArity name
    [d| instance TypeArity $(conT name) where
            arity _ = at |]
#-}