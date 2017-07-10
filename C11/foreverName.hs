import Control.Monad
main = forever (do
                print "Can you tell me your name?"
                name <- getLine
                print ("Hello " ++ name))