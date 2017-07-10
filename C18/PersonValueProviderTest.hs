{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -ddump-splices #-}
import PersonValueProvider
import Language.Haskell.TH.Quote

song_ZHANG = [personJSON| {"name" : "Song ZHANG", "age":26} |]
nuo_LI     = [personJSON_file|person.json|] -- 文件名无空格时牛津括号内也不能有空格