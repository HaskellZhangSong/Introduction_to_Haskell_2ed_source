import System.Environment

main = do
        args <- getArgs
        case args of
                [] -> print "please input some arguments"
                arg -> mapM_ print args