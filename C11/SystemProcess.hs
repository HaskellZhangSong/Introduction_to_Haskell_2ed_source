{-# LANGUAGE StandaloneDeriving #-}

import System.Process
import GHC.IO.Handle
import System.IO

deriving instance Show (CmdSpec)
deriving instance Show (StdStream)
deriving instance Show (CreateProcess)

command1 = do
         (Nothing,Nothing,Nothing,d) <-
                              createProcess (proc "ls" [])
         return ()

command2 = do
         (Nothing,Just b,Nothing,d) <-
                              createProcess (proc "ls" []) { std_out = CreatePipe }
         l <- hGetContents b
         return l
