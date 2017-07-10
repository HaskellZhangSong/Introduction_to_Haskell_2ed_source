import Test.QuickCheck
import Control.Monad

newtype Exp = Exp String deriving (Show ,Eq)

instance Arbitrary Exp where
  arbitrary = do
    n <- choose (0, 2) :: Gen Int
    case n of
      0 -> do
        unaryOp <-
          elements ["", "+", "-", "sin ", "cos ", "log ", "ln ", "sqrt "]
        (Exp exp) <- arbitrary :: Gen Exp
        return (Exp (unaryOp ++ exp))
      1 -> do
        Exp exp <- arbitrary :: Gen Exp
        let bracketExp = "(" ++ exp ++ ")"
        n1 <- choose (0, 1) :: Gen Int
        case n1 of
          0 -> do
            binaryOp <- elements ["+", "-", "*", "/", "^"]
            (Exp exp1) <- arbitrary :: Gen Exp
            return (Exp (bracketExp ++ binaryOp ++ exp1))
          1 -> do
            return (Exp bracketExp)
      2 -> do
        num <-
          oneof [liftM show (choose (-20, 20) :: Gen Int), elements ["pi", "e"]]
        n1 <- choose (0, 1) :: Gen Int
        case n1 of
          0 -> do
            binaryOp <- elements ["+", "-", "*", "/", "^"]
            (Exp exp1) <- arbitrary :: Gen Exp
            return (Exp (num ++ binaryOp ++ exp1))
          1 -> do
            return (Exp (num))

