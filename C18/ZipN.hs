{-# LANGUAGE TemplateHaskell #-}
module ZipN where
import Language.Haskell.TH

genPE :: String -> Int -> ([Pat], [Exp])
genPE s n = let ns = [s++ (show i) | i <- [1..n]]
                in (map (VarP. mkName) ns, map (VarE. mkName) ns)
                
genBT :: String -> Int -> ([TyVarBndr], [Type])
genBT s n = let ns = [s++ (show i) | i <- [1..n]]
                in (map (PlainTV. mkName) ns, map (VarT. mkName) ns)

applyCurryT :: [Type] -> Type
applyCurryT [x] = x
applyCurryT (x:xs) = AppT (AppT ArrowT x) (applyCurryT xs)

applyConT :: [Type] -> Type
applyConT [x] = x
applyConT (x1:x2:xs) = applyConT ((AppT x1 x2):xs)

appExp :: [Exp] -> Exp
appExp [x] = x 
appExp (x:y:xs) = appExp (AppE x y : xs)

zipN :: Int -> Q [Dec]
zipN n = return [sigDec, funDec]
    where
       -- 生成n个类型变量a1, a2, a3 ...
        (b, t)  = genBT "a" n
       -- 生成匹配的模式与表达式
        (py,ey) = genPE "y" n
        (px,ex) = genPE "x" n
        (pxs,exs) = genPE "xs" n
       -- 生成函数名
        funcname = (mkName ("zip" ++ show n))
       -- 构造类型[a0] -> [a1] -> ... -> [(a1,a2,...)]
        typ = applyCurryT $ map (AppT ListT) t ++ 
                            [(AppT ListT (applyConT (TupleT n : t)))]
       -- 构造类型签名
        sigDec = SigD funcname (ForallT b [] typ)
       -- 构造函数声名
        funDec = FunD funcname [Clause py (NormalB body) []]
        body = CaseE (TupE ey) 
                 [Match (TupP (map (\(x,xs) -> ConP '(:) [x,xs]) 
                                   (zip px pxs)))
                        (NormalB (AppE (AppE (ConE '(:)) (TupE ex)) 
                                 (appExp (VarE funcname : exs))))
                        [],
                  Match WildP (NormalB (ConE '[])) []]   

zips = fmap concat (sequence ([zipN x | x <- [4..10]]))