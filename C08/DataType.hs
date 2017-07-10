-- DataType.hs

data Day = Mon | Tue | Wed | Thu | Fri | Sat | Sun 
                                    deriving (Show,Read, Eq,Ord,Enum)
{-                  
tomorrow :: Day -> Day
tomorrow Mon = Tue
tomorrow Tue = Wed
tomorrow Wed = Thu
tomorrow Thu = Fri
tomorrow Fri = Sat
tomorrow Sat = Sun
tomorrow Sun = Mon
-}

tomorrow Sun = Mon
tomorrow d = succ d

yesterday Mon = Sun
yesterday d  = pred d

type Name   = String
type Author = String
type ISBN   = String
type Price  = Float

data Book = Book {
         name   :: Name,
         author :: Author,
         isbn   :: ISBN,
         price  :: Price
       }
{-
incrisePrice (b1,b2) b pri =
          ((b:b1),Book (name b) (author b) (isbn b) (price b + pri))
-}
{-
incrisePrice (b1,b2) (Book nm ath isbn prc) pri =
          ((Book nm ath isbn prc):b1 ,(Book nm ath isbn (prc+pri)):b2)
-}

{-
increasePrice (b1,b2) b@(Book nm ath isbn prc) pri = 
                           (b:b1,(Book nm ath isbn (prc+pri)):b2)
-}

increasePrice   :: ([Book], [Book]) -> Book -> Price -> ([Book], [Book])
increasePrice (b1,b2) b pri = (b:b1, (b{price=pri}):b2)

data Pair a b = Pair a b

pfirst (Pair a b) = a
psecond (Pair a b) = b

---------------------------------
---------------------------------
data Nat = Zero | Succ Nat deriving (Show,Eq,Ord)

natToint :: Nat -> Int
natToint Zero = 0
natToint (Succ n) = 1 + natToint n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n-1))

{-
add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ m) n = Succ (add m n)
-}

add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ m) n = add m (Succ n)

---------------------------------
---------------------------------
data Shape = Circle {
               radius :: Float
             } | Rect {
               len :: Float,
               width :: Float
             } deriving (Show,Eq)

area :: Shape -> Float
area (Circle r) = pi * r^2
area (Rect a b) = a * b

data Person = Person {
    pname :: String,
    age  :: Int,
    sex  :: Bool  }

showPerson :: Person -> String
showPerson (Person {pname = str, sex = s}) = str ++ show s


data BoolExp = TRUE | FALSE | IF BoolExp BoolExp BoolExp deriving (Show,Eq)

eval :: BoolExp -> Bool
eval TRUE  = True
eval FALSE = False
eval (IF con b1 b2) | eval con == True  = eval b1
                    | eval con == False = eval b2
                    
eval' :: [BoolExp] -> Bool 
eval' [TRUE]  = True
eval' [FALSE]  = False
eval' ((IF TRUE  b1 b2):xs) = eval' (b1:xs)
eval' ((IF FALSE b1 b2):xs) = eval' (b2:xs)
eval' (l@(IF con b1 b2):xs) = eval' (con:l:xs)
eval' (TRUE:(IF con b1 b2):xs)  = eval' (b1:xs)
eval' (FALSE:(IF con b1 b2):xs) = eval' (b2:xs)

test = IF (IF FALSE FALSE TRUE) (IF FALSE TRUE FALSE) FALSE

---------------------------------
---------------------------------
data List a = Nil | Cons a (List a) deriving (Eq,Show)

mylistToList Nil = []
mylistToList (Cons x xs) = x:(mylistToList xs)

listToMylist [] = Nil
listToMylist (x:xs) = Cons x (listToMylist xs)