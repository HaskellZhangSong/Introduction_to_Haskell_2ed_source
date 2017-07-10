{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Environment (getArgs)
import Text.Printf
import Control.Exception
import Data.Word (Word8)
import Data.Bits (shiftL)
import Data.List (foldl')
import System.IO
import Data.String (fromString)
import qualified Data.Text as T
import qualified Data.Text.Encoding as Encode (decodeUtf8)
import qualified Data.ByteString as DB
import qualified Data.ByteString.Char8 as DBC (putStrLn)

data WordIdx = WordIndex {
                word      :: T.Text,
                offset    :: Int,
                expLen    :: Int
              } deriving Show

byteToInt :: [Word8] -> Int
byteToInt bs = foldl' (\x y -> shiftL (fromIntegral x) 8 + fromIntegral y) 0 bs

getIndexList :: DB.ByteString -> [WordIdx]
getIndexList "" = []
getIndexList str = WordIndex w (byteToInt o) (byteToInt e) : getIndexList left
                 where 
                       w    = Encode.decodeUtf8 $ DB.takeWhile (/= 0) str
                       o    = DB.unpack $ DB.take 4 (DB.drop 1 off)
                       e    = DB.unpack $ DB.take 4 (DB.drop 5 off)
                       off  = DB.dropWhile (/= 0) str
                       left = DB.drop 9 off

searchWord :: T.Text -> [WordIdx] -> Maybe WordIdx
searchWord str [] = Nothing
searchWord str xs | wrd < str = searchWord str behind
                  | wrd > str = searchWord str front
                  | otherwise    = Just b                  
                where (front,b:behind) = splitAt (length xs `div` 2) xs
                      wrd = T.toLower (word b)

main :: IO ()
main = do 
        arg <- getArgs
        case arg of 
            []     -> print "Usage: Dict <word>"
            (a:_)  -> do
               idctIdx <- DB.readFile "./Dict/longman.idx"
               let is = getIndexList idctIdx
               let result = searchWord (fromString a) is
               case result of
                 Nothing  -> printf "Word \"%s\" not found" a
                 Just wrd -> do
                      bracket (openFile "./Dict/longman.dict" ReadMode) hClose $ \inh -> do
                                  hSeek inh AbsoluteSeek (toInteger $  offset wrd)
                                  hSetEncoding inh utf8
                                  explanation <- DB.hGet inh (expLen wrd)
                                  DBC.putStrLn explanation
