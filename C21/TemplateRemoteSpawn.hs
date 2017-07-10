{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Distributed.Process
import Control.Distributed.Process.Node (initRemoteTable, runProcess)
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Backend.SimpleLocalnet

foo :: SendPort String -> Process String
foo sp = do
  sendChan sp "World"
  return "Hello "

remotable ['foo]

main :: IO ()
main = do
  backend <- initializeBackend "localhost" "2000" $ __remoteTable initRemoteTable
  node <- newLocalNode backend
  runProcess node $ do
    nid <- getSelfNode
    (sp, rp) <- newChan
    res1 <- call $(functionTDict 'foo) nid ($(mkClosure 'foo) sp)
    res2 <- receiveChan rp
    liftIO (putStrLn $ res1 ++ res2)
