module Calculator where
import Control.Monad.State

data Lit = Val Float | Const String | Empty deriving (Eq,Show)

data Op = Posi | Nega | Plus | Minu 
         | Mult | Divi | Power
         | Log  | Ln   | Sin   | Cos | Sqrt 
         | L_Par | R_Par 
         | OpBottom
        deriving (Eq, Show)

data Order = Unary | Binary | Null | Bottom

nary :: Op -> Order
nary op = case op of
        Plus -> Binary
        Minu -> Binary
        Mult -> Binary
        Divi -> Binary
        Power-> Binary
        Posi -> Unary
        Nega -> Unary
        Log  -> Unary
        Ln   -> Unary
        Sin  -> Unary
        Cos  -> Unary
        Sqrt -> Unary
        OpBottom -> Bottom
        _    -> Null

priority :: Op -> Int
priority op = case op of
                Plus -> 1
                Minu -> 1
                Mult -> 2
                Divi -> 2
                Log  -> 3
                Sin  -> 3
                Cos  -> 3
                Posi -> 4 
                Nega -> 4
                Sqrt -> 4
                Power-> 5
                _    -> 0

type LitOp = Either Lit Op

type Stack = ([LitOp],[LitOp])

evaluate :: Op -> LitOp -> LitOp -> State Stack ()
evaluate op (Left (Val f1)) (Left (Val f2)) = case op of
                                Plus -> push $ lv (f1+f2)
                                Minu -> push $ lv (f1-f2)
                                Mult -> push $ lv (f1*f2)
                                Divi -> push $ lv (f1/f2)
                                Power-> push $ lv (f1**f2)
evaluate op (Left (Val f1)) (Left Empty) = case op of
                                Posi -> push $ lv f1
                                Nega -> push $ lv (-f1)
                                Log  -> push $ lv (logBase 2 f1)
                                Ln   -> push $ lv (log f1)
                                Sin  -> push $ lv (sin f1)
                                Cos  -> push $ lv (cos f1)
                                Sqrt -> push $ lv (sqrt f1)

lv :: Float -> LitOp
lv x = Left $ Val x

pop0,pop1 :: State Stack LitOp
pop0 = state $ \(ls,rs) -> case ls of
                              []     -> error "Number stack underflow"
                              (h:hs) -> (h,(hs,rs))
pop1 = state $ \(ls,rs) -> case rs of
                              []     -> error "Operator stack underflow"
                              (h:hs) -> (h,(ls,hs))

push :: LitOp -> State Stack ()
push (Left (Const "pi")) = push $ lv 3.1415926
push (Left (Const "e" )) = push $ lv 2.7182812
push (Left (Const c )) = error $ "Unkown Constant"++c
push l@(Left  a) = state $ \(ls,rs) -> ((), (l:ls,rs))
push r@(Right a) = state $ \(ls,rs) -> ((), (ls,r:rs))

pushIn  :: LitOp -> State Stack ()
pushIn l@(Left    num) = push l                    
pushIn p@(Right L_Par) = push p
pushIn p@(Right R_Par) = do
                Right top <- pop1
                case nary top of
                    Null  -> return ()
                    Unary -> do
                        f1 <- pop0                     
                        evaluate top f1 (Left Empty) 
                        pushIn p
                    Binary-> do
                        f1 <- pop0
                        f2 <- pop0
                        evaluate top f2 f1
                        pushIn p
                    Bottom -> error "Excepted Left Bracket\n"
pushIn o@(Right     op) = do
                case nary op of
                    Unary  -> push o
                    Binary -> do
                        Right top <- pop1
                        case nary top of
                            Unary -> do
                                let pri = priority top > priority op in
                                    case pri of
                                        True -> do
                                           f1 <- pop0
                                           evaluate top f1 (Left Empty)
                                           pushIn o
                                        False -> do
                                           push (Right top)
                                           push o
                            Binary -> do
                                case op of
                                    Power -> do
                                           push (Right top)
                                           push o
                                    _     -> do
                                           let pri=priority top >= priority op
                                           case pri of
                                                True  -> do
                                                     f1 <- pop0
                                                     f2 <- pop0
                                                     evaluate top f2 f1
                                                     pushIn (Right op)
                                                False -> do 
                                                     push (Right top)
                                                     push o
                            _      -> do   --- L_Par and OpBottom
                                        push (Right top)
                                        push o
calc :: [LitOp] -> State Stack LitOp
calc [] = do
            Right op <- pop1
            case nary op of
                Bottom -> pop0
                Unary  -> do
                        f1 <- pop0
                        evaluate op f1 (Left Empty)
                        calc []
                Binary -> do
                        f1 <- pop0
                        f2 <- pop0
                        evaluate op f2 f1
                        calc []
                Null -> error "Excepted right bracket"

calc (t:ts) = do
        pushIn t
        calc ts

inits :: ([LitOp],[LitOp])
inits = ([], [Right OpBottom])

test1 = [Right Nega, Left (Const "pi")]

test2  = [Right L_Par, Left (Val 5.0),Right Plus, Left (Val 6),Right R_Par, Right Mult, Left (Val 3.0)] -- (5+6)*3

test3 = [Right Log , Right L_Par,Left (Val 8),Right Plus, Left (Val 8), Right R_Par, Right Plus , Left (Val 5)]  -- log (8+8) + 5 

test4 = [Right Log ,Left (Val 8),Right Power, Left (Val 2), Right Plus , Left (Val 5)]  -- log 8^2 + 5

test5 = [Right Sqrt , Left (Val 4),Right Plus,Left (Val 4)] --sqrt 4 + 4

          