data BoolExp = TRUE | FALSE | IF BoolExp BoolExp BoolExp 
                                deriving (Eq, Show)

eval :: BoolExp -> Bool
eval TRUE  = True
eval FALSE = False
eval (IF con b1 b2) | eval con == True  = eval b1
                    | eval con == False = eval b2
