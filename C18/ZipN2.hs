{-# LANGUAGE TemplateHaskell #-}
module ZipN2 where

import Language.Haskell.TH
import Language.Haskell.TH.Lib
import Control.Monad
import Data.List
import Control.Applicative

genBT :: String -> Int -> Q ([TyVarBndr], [TypeQ])
genBT name n = do
	let ns = [name++ (show i) | i <- [1..n]]
	tvb <- sequence $ map (return.plainTV.mkName) ns
	typ <- sequence $ map (return.varT.mkName) ns
	return (tvb,typ)

genPE :: String -> Int -> Q ([PatQ],[ExpQ])
genPE name n = do 
	let ns = [name++ (show i) | i <- [1..n]]
	pat <- sequence $ map (return.varP.mkName) ns
	exp <- sequence $ map (return.varE.mkName) ns
	return (pat,exp)

applyCurryTQ :: [TypeQ] -> TypeQ
applyCurryTQ  = foldr1 (\t1 -> appT (appT arrowT t1))

applyConTQ :: [TypeQ] -> TypeQ
applyConTQ xs = foldl1 appT xs

applyExpQ :: [ExpQ] -> ExpQ
applyExpQ = appsE

zipN :: Int -> DecsQ
zipN n = do
           -- 函数名为zip与一个数字     
	let name = mkName ("zip" ++ show n)
	       -- 生成n个类型变量a1, a2, a3 ...     
	(tvb, tvar) <- genBT "a" n      -- tvar :: [Q Type]     
	       -- 构造n个列表类型[a1], [a2], [a3] ...     
	let listvar = map (appT listT) tvar
	       -- 构造[(a1,a2...)]类型     
	let lstuple = appT listT (applyConTQ (tupleT n : tvar)) 
	       -- [a1] -> [a2] -> ... [an] -> [(a1,a2,...)]     
	let typ     = applyCurryTQ (listvar ++ [lstuple])
	       -- zipn :: forall a1 a2 ... . [a1] -> [a2] ... [(a1,a2...)]     
	sig         <- sigD name (forallT tvb (return []) typ)
           -- 构造匹配模式与对应的变量     
	(py, pyv)   <- genPE "y" n
	(px, pxv)   <- genPE "x" n
	(pxs, pxsv) <- genPE "xs" n
	let pcons x xs = [p| $x : $xs |]
           -- case模式匹配的模式，这里为一个多元元组     
	let matchp = tupP (zipWith pcons px pxs)
           -- case模式匹配的函数体     
	let matchb = [e| $(tupE pxv) : $(applyExpQ (varE name : pxsv))|]
	let body = normalB [e| case $(tupE pyv) of 
								    $matchp -> $matchb 
								    _       -> []      |]
	fun <- funD name [(clause py body [])]
	return [sig, fun]

zips = fmap concat (sequence ([zipN x | x <- [4..10]]))
