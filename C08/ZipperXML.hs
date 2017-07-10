{-# LANGUAGE QuasiQuotes,OverloadedStrings  #-}
import Prelude hiding (readFile)
import Text.RawString.QQ
import Data.ByteString.Lazy hiding (length,head)
import Text.XML.Cursor
import Text.XML
import Data.Function
xml :: ByteString
xml = [r|<html>
    <head>
        <title>My<b> Title1 </b></title>
        <title>My<b> Title2 </b></title>
        <title>My<b> Title3 </b></title>
    </head>
    <body>
        <p>Foo</p>
        <p>Bar</p>
    </body>
</html>|]

cursor = fromDocument $ parseLBS_ def xml
