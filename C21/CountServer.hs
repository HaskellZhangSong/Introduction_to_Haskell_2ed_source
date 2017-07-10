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
import Control.Distributed.Process.ManagedProcess as M
import Control.Distributed.Process.Extras (ExitReason(..))
import Control.Distributed.Process.Extras.Time (Delay(..))
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Node as P (initRemoteTable, runProcess)
import Control.Distributed.Process.Backend.SimpleLocalnet

-- Experimental
import Control.Concurrent (threadDelay)
import System.Environment
import Control.Distributed.Backend.P2P as P2P

data CountingCommand = AddOne | SetCount Int
  deriving (Generic, Typeable, Binary)
data InfoCommand = GetCount | GetCountSquare
  deriving (Generic, Typeable, Binary)
data ControlCommand = ExitServer
  deriving (Generic, Typeable, Binary)

data CountServer = CountServer
  { countServerPid  :: ProcessId
  , countingCmdPort :: ControlPort CountingCommand }

newCountServer :: Process CountServer
newCountServer = do
  (sp, rp) <- newChan
  CountServer
    <$> spawnLocal (server sp)
    <*> receiveChan rp

addOne :: CountServer -> Process ()
addOne s = sendControlMessage (countingCmdPort s) AddOne

setCount :: CountServer -> Int -> Process ()
setCount s c = sendControlMessage (countingCmdPort s) (SetCount c)

getCount :: CountServer -> Process Int
getCount s = M.call (countServerPid s) GetCount

getCountSquare :: CountServer -> Process Int
getCountSquare s = M.call (countServerPid s) GetCountSquare

exitServer :: CountServer -> Process ()
exitServer s = cast (countServerPid s) ExitServer

getCountRpcChan :: CountServer -> Process Int
getCountRpcChan s = syncCallChan (countServerPid s) GetCount

server :: SendPort (ControlPort CountingCommand) -> Process ()
server sp = do
  cc <- newControlChan
  sendChan sp $ channelControlPort cc

  let myDef = defaultProcess
        { apiHandlers =
            [ handleControlChan cc handleCounting
            , handleCallFrom handleInfo
            , handleCast_ $ \ExitServer -> stop_ ExitNormal
            , handleRpcChan $ \s chan GetCount -> sendChan chan s >> continue s ]
        , shutdownHandler = \s r -> say $ "[Stopped] count: " ++ show s ++ ", reason: " ++ show r
        , unhandledMessagePolicy = Log
        }
  serve () (\() -> return $ InitOk (0 :: Int) Infinity) myDef
  where
    handleCounting s AddOne = continue (succ s)
    handleCounting s (SetCount ns) = continue ns
    handleInfo s _   GetCount = reply s s
    handleInfo s ref GetCountSquare = do
      void $ spawnLocal $ do
        liftIO $ putStrLn "now calculating..."
        replyTo ref (s * s)
      noReply_ s

main :: IO ()
main = do
  backend <- initializeBackend "localhost" "2000" $ __remoteTable initRemoteTable
  node <- newLocalNode backend
  P.runProcess node $ do
    pid <- spawnLocal server
    addOne pid
    addOne pid
    getCount pid >>= liftIO . print
    setCount pid 1
    getCount pid >>= liftIO . print
