-- ParaListComp.hs
{-# LANGUAGE ParallelListComp, TransformListComp #-}
import GHC.Exts

table = [("Hangzhou", "MP4", 243),
         ("Hangzhou", "CD" , 925),
         ("Beijing" , "MP4", 157),
         ("Beijing" , "CD" , 536),
         ("Shanghai", "MP4", 784),
         ("Shanghai", "CD" , 766)]

analysis = [(the product, sum cost)|
                (city, product, cost) <- table,
                then group by product using groupWith,
                then sortWith by (sum cost)]