{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Main where

import System.Environment (getArgs)
import GHC.Generics
import Data.Binary
import Data.Rank1Typeable

import Control.Monad

import Control.Distributed.Process
import Control.Distributed.Process.Node (initRemoteTable, runProcess)
import Control.Distributed.Process.Backend.SimpleLocalnet

data Ping = Ping ProcessId
  deriving (Generic, Typeable, Binary)
data Pong = Pong ProcessId
  deriving (Generic, Typeable, Binary)

pingPong :: Process ()
pingPong = forever $ do
  self <- getSelfPid
  Ping pid <- expect
  say $ "Ping from " ++ show pid
  send pid (Pong self)

pingPong' :: Process ()
pingPong' = forever $ do
  self <- getSelfPid
  receiveWait
    [ match $ \(Ping pid) -> send pid (Pong self)
    , match $ \(Pong pid) -> send pid (Ping self) ]

main :: IO ()
main = do
  [port] <- getArgs
  backend <- initializeBackend "localhost" port initRemoteTable
  node <- newLocalNode backend
  peers <- findPeers backend 1000000
  runProcess node $ do
    self <- getSelfPid
    register "pingPongProc" self
    forM_ peers $ \nid -> nsendRemote nid "pingPongProc" ()
    pingPong
