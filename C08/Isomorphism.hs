data ThreeNum = One | Two | Three
data Level = Low | Middle | High

f :: ThreeNum -> Level
f One = Low
f Two = Middle
f Three = High

g :: Level -> ThreeNum
g Low = One
g Middle = Two
g High = Three

data Unit = Unit
data List a =  Nil  | Cons a (List a)
data Nat    =  Zero | Succ Nat

list2Nat Nil = Zero
list2Nat (Cons x xs) = Succ (list2Nat xs)

nat2List Zero = Nil
nat2List (Succ n) = Cons Unit (nat2List n)
