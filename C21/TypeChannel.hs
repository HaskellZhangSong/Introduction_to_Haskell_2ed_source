module Main where

import Control.Monad

import Control.Distributed.Process
import Control.Distributed.Process.Node (initRemoteTable, runProcess)
import Control.Distributed.Process.Backend.SimpleLocalnet

example :: Process ()
example = do
  (sp, rp) <- newChan
  void $ spawnLocal (sendChan sp "Hello world")
  receiveChan rp >>= liftIO . putStrLn

main :: IO ()
main = do
  backend <- initializeBackend "localhost" "8000" initRemoteTable
  node <- newLocalNode backend
  runProcess node example
