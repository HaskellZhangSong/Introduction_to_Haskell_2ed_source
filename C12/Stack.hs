import Control.Monad.State
type Stack = [Int]

pop :: State Stack Int
pop = state $ \(x:xs) -> (x,xs)

peek :: State Stack Int
peek = state $ \(x:xs) -> (x,x:xs)

push :: Int -> State Stack ()
push i = state $ \xs -> ((), i:xs)

addStack :: State Stack ()
addStack = do
        a1 <- pop
        a2 <- pop
        let a3 = a1+a2
        push a3
