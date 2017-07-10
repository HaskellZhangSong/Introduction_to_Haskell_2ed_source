module Main where

import Data.Binary
import Data.ByteString.Lazy
import Data.Rank1Dynamic
import Control.Monad

import Control.Distributed.Static hiding (initRemoteTable)
import Control.Distributed.Process
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Node (initRemoteTable, runProcess)
import Control.Distributed.Process.Backend.SimpleLocalnet

sendStr :: SendPort String -> Process ()
sendStr sp = sendChan sp "Hello world"

sendStrStatic :: Static (SendPort String -> Process ())
sendStrStatic = staticLabel "$sendStr"

decodeSPStatic :: Static (ByteString -> SendPort String)
decodeSPStatic = staticLabel "$decodeSP"

sendStrClosure :: SendPort String -> Closure (Process ())
sendStrClosure sp = closure decoder (encode sp)
  where decoder :: Static (ByteString -> Process ())
        decoder = sendStrStatic `staticCompose` decodeSPStatic

rtable :: RemoteTable
rtable =
    registerStatic "$sendStr"  (toDynamic sendStr)
  . registerStatic "$decodeSP" (toDynamic (decode :: ByteString -> SendPort String))
  $ initRemoteTable

main :: IO ()
main = do
  backend <- initializeBackend "localhost" "8000" rtable
  node <- newLocalNode backend
  runProcess node $ do
    (sp, rp) <- newChan
    nid <- getSelfNode
    void $ spawn nid (sendStrClosure sp)
    receiveChan rp >>= liftIO . Prelude.putStrLn
