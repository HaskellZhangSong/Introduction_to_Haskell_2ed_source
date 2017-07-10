module Main where

import Control.Distributed.Process
import Control.Distributed.Process.Node (initRemoteTable, runProcess)
import Control.Distributed.Process.Backend.SimpleLocalnet

main :: IO ()
main = do
  backend <- initializeBackend "localhost" "8000" initRemoteTable
  node <- newLocalNode backend
  runProcess node $ do
    self <- getSelfPid
    send self "Hello world"
    "Hello world" <- expect
    return ()
