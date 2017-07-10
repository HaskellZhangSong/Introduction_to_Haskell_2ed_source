{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Monad

import Control.Distributed.Process
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Node (initRemoteTable, runProcess)
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Distributed.Process.Async

doTask :: (Double, Double) -> Process Double
doTask (n, d) = do
  when (d == 0) $ die "Task failed!"
  return (n / d)

remotable ['doTask]

mainProcess :: Process ()
mainProcess = do
  nid <- getSelfNode
  let createTask :: Double -> Double -> AsyncTask Double
      createTask n d =
        remoteTask $(functionTDict 'doTask) nid ($(mkClosure 'doTask) (n, d))
  h1 <- asyncLinked $ createTask 1 2
  h2 <- asyncLinked $ createTask 2 0
  AsyncDone 0.5 <- wait h1
  AsyncFailed _ <- wait h2
  return ()

main :: IO ()
main = do
  backend <- initializeBackend "localhost" "2000" $ __remoteTable initRemoteTable
  node <- newLocalNode backend
  runProcess node mainProcess
