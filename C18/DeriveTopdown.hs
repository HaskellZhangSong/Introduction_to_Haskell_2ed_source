{-# LANGUAGE TemplateHaskell #-}
module DeriveTopdown (deriveTopdown, derivings) where
import Language.Haskell.TH
import Control.Monad.State
import Control.Monad.Trans (lift)
import Data.List (foldl')
import qualified GHC.Generics as G

getTyVarCons :: Name -> Q ([TyVarBndr], [Con])
getTyVarCons name = do
    info <- reify name
    case info of 
        TyConI dec ->
            case dec of
                DataD    _ _ tvbs cons _ -> return (tvbs,cons)
                NewtypeD _ _ tvbs con  _ -> return (tvbs,[con])
                TySynD   _ vars type'    -> undefined
                _ -> error "must be a data, newtype definition"
        _ -> error "bad type name, quoted name is not a type!"

getTypeNames :: Type -> [Name]
getTypeNames (ConT n) = [n]
getTypeNames (AppT t1 t2) = getTypeNames t1 ++ getTypeNames t2
getTypeNames _ = []

third (a,b,c) = c

getCompositeType :: Con -> [Name]
getCompositeType (NormalC n sts)    = concatMap getTypeNames (map snd sts)
getCompositeType (RecC    n vars)   = concatMap getTypeNames (map third vars)
getCompositeType (InfixC st1 n st2) = concatMap getTypeNames [snd st1, snd st2]
getCompositeType _ = undefined

getTVBName :: TyVarBndr -> Name
getTVBName (PlainTV  name  ) = name
getTVBName (KindedTV name _) = name

     -- 假想我们要为(A a b G)类型生成Eq类型类的实例，即cn为''Eq，tn为''A     
gen :: Name -> Name -> StateT [Type] Q [Dec]
gen cn tn = do
         -- 得到所需要的类型变量与构造器     
    (tvbs,cons) <- lift $ getTyVarCons tn
    let typeNames = map getTVBName  tvbs
         -- 然后使用把''A, a, b, G应用起来变成(A a b G)     
    instanceType <- lift $ foldl' appT (conT tn) $ map varT typeNames
         -- 接下来生成所需要的上下为，Eq a, Eq b .....     
    let context = if cn == ''G.Generic      -- Generic实例的生成不需要这个上下文     
                    then []
                         -- 会得到[Eq a, Eq b]     
                    else (map (AppT (ConT cn)) (map VarT typeNames))
         -- 把context放到一个元组中(Eq a , Eq b , ...)     
    let context_in_tuple = foldl1 AppT $ (TupleT (length context)) : context
         -- 判断该类型是不是已经为cn的实例     
    isMember <- if tvbs == []
                  then lift $ isInstance cn [instanceType]
                       -- 下面的代码并不会工作\footnote{这主要是由于\api{}中{\nc{isInstance}}的实现没有考虑类型类上下文，比如我们请求{\nc{Eq a => Eq [a]}}是不是{\nc{Eq}}类型类的实例结果会是{\nc{False}}，但对于我们这个简单的实现影响不大。}     
                  else lift $ isInstance cn [ForallT tvbs [] instanceType]
         -- 从状态Monad中取出已经生成过的类型     
    table <- get
         -- 如果tn已经为cn的实例或者已经生成过了就不返回任何结果     
    if isMember || elem instanceType table
      then return []
           -- 否则返回这个类型类实例的声明，并把新生成的类型用modify函数加入到状态中去     
           -- standalone driving: deriving instance (Eq a , Eq b) => Eq (A a b G)     
      else do
        let c = [StandaloneDerivD [context_in_tuple] (AppT (ConT cn)  instanceType)]
        modify (instanceType:)
             -- 得到这个数据构造器需要的类型，也就是A a b C中的C     
        let names = concatMap getCompositeType cons
             -- 递归地向下生成该类型类的实例     
        xs <-  mapM (\n -> gen cn n) names
        return $ concat xs ++ c

deriveTopdown :: Name -> Name -> Q [Dec]
deriveTopdown cn tn = evalStateT (gen cn tn) []

derivings :: [Name] -> Name -> Q [Dec]
derivings cnms t = fmap concat (sequenceA $ map (\x -> deriveTopdown x t) cnms)
