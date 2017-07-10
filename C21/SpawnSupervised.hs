{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Main where

import GHC.Generics
import Data.Binary
import Data.Rank1Typeable
import Control.Monad

import Control.Distributed.Process
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Node (initRemoteTable, runProcess)
import Control.Distributed.Process.Backend.SimpleLocalnet

data Task = Task ProcessId Double Double
  deriving (Generic, Typeable, Binary)

doTask :: Double -> Double -> Process Double
doTask n d = do
  when (d == 0) $ die "Denominator cannot be zero!"
  return (n / d)

worker :: NodeId -> Process ()
worker nid = forever $ do
  Task pid n d <- expect
  res <- callLocal $ doTask n d
  send pid res

remotable ['worker]

main :: IO ()
main = do
  backend <- initializeBackend "localhost" "2000" $ __remoteTable initRemoteTable
  node <- newLocalNode backend
  runProcess node $ do
    nid <- getSelfNode
    pid <- getSelfPid
    (wpid, _) <- spawnSupervised nid ($(mkClosure 'worker) nid)
    send wpid $ Task pid 5 2
    receiveWait
      [ match $ \(res :: Double) ->
          liftIO $ putStrLn $ "Succeed: " ++ show res
      , match $ \(pmn :: ProcessMonitorNotification) ->
          liftIO $ putStrLn $ "Worker died: " ++ show pmn ]
