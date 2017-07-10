import Text.Parsec
import qualified Text.Parsec.Token as T
import Text.Parsec.Language (emptyDef)

lexer :: T.TokenParser ()
lexer = T.makeTokenParser emptyDef

lexeme :: Parsec String () a -> Parsec String () a
lexeme = T.lexeme lexer

float :: Parsec String () Double
float = T.float lexer

chars :: Parsec String () [Char]
chars = do 
        c1 <- lexeme $ char 'a'
        c2 <- lexeme $ char 'b'
        return [c1,c2]

float1 :: Parsec String () Double
float1 = do
        f <- lexeme sign
        n <- T.float lexer
        return (f n)

sign :: Num a => Parsec String () (a -> a) 
sign =  (char '-' >> return negate)
            <|> (char '+' >> return id)
            <|> return id
            
data Exp = Add Exp Exp | Mul Exp Exp | Val Double deriving (Eq,Show)

{-
parseExp :: Parsec String () Exp
parseExp = do
        e1 <- parseExp
        char '+'
        e2 <- parseMul
        return (Add e1 e2)
        <|> parseMul
-}
eval (Val v)  = v
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2

parseExp :: Parsec String () Exp
parseExp = do
        e1 <- parseMul
        e2 <- parseExp'
        case e2 of
            Nothing -> return e1
            Just e  -> return (e e1)

-- $Exp' ::= + Mul Exp'| epsilon$

parseExp' :: Parsec String () (Maybe (Exp -> Exp))
parseExp' = try (do
        char '+'
        e1 <- parseMul
        e2 <- parseExp'
        case e2 of
            Nothing -> return (Just (\e -> Add e e1))
            Just e -> return (Just (\e' -> e (Add e' e1) )))
            <|> (return Nothing )

-- $Mul ::= Num Mul'$

parseMul :: Parsec String () Exp
parseMul = do
        e1 <- parseNum
        e2 <- parseMul'
        case e2 of
            Nothing -> return e1
            Just e  -> return (e e1) 

-- $Mul' ::= * Num Mul' | epsilon$

parseMul' :: Parsec String () (Maybe (Exp -> Exp ))
parseMul' = try (do
        char '*'
        e1 <- parseNum
        e2 <- parseMul'
        case e2 of
            Nothing -> return (Just (\e -> Mul e e1))
            Just e  -> return (Just (\e' -> e (Mul e' e1)))) <|> return Nothing

-- $Num ::=\ (Exp)\ |\ Number$

parseNum :: Parsec String () Exp
parseNum = try (do
        char '('
        e1 <- parseExp
        char ')'
        return e1) <|> (do {num <- float1; return (Val num)})

calculate str  = case runParser parseExp () "" str of
                      Right exp -> eval exp
                      Left _ -> error "error"
