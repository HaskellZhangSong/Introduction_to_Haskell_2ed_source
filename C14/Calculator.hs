import Text.Parsec
import qualified Text.Parsec.Token as T
import Text.ParserCombinators.Parsec.Language
import Text.Parsec.String

data Exp = Plus Exp Exp | Minu Exp Exp | Mult Exp Exp | Divi Exp Exp
         | Power Exp Exp | Nega Exp | Posi Exp | Sqrt Exp |Log Exp | Ln Exp 
         | Sin Exp | Cos Exp |  Val Double deriving (Eq , Show) 

eval :: Exp -> Double
eval (Plus e1 e2) = eval e1 + eval e2
eval (Minu e1 e2) = eval e1 - eval e2
eval (Mult e1 e2) = eval e1 * eval e2
eval (Divi e1 e2) = eval e1 / eval e2
eval (Power e1 e2) = eval e1 ** eval e2
eval (Nega e1) = negate $ eval e1
eval (Posi e1) = eval e1
eval (Log  e1) = logBase 2 (eval e1)
eval (Ln   e1) = log $ eval e1
eval (Sin  e1) = sin $ eval e1
eval (Cos  e1) = cos $ eval e1
eval (Sqrt e) = sqrt $ eval e
eval (Val  e) = e

lexer :: T.TokenParser ()
lexer = T.makeTokenParser emptyDef

lexeme :: Parsec String () a -> Parsec String () a
lexeme = T.lexeme lexer

constant :: Parsec String () Double
constant = do
        choice [lexeme $ string "pi" >> return 3.1415926,
                lexeme $ string "e"  >> return 2.71828 ]

float :: Parsec String () Double
float = do
        n <- T.float lexer
        return n

number :: Parsec String () Double
number = try (do 
        int <- T.integer lexer
        return (fromIntegral int)) <|> try float <|> constant

parseExp :: Parser Exp
parseExp = do       
        e1 <- parseMul
        e2 <- parseExp'
        case e2 of 
            Nothing -> return e1
            Just e  -> return (e e1)

parseExp' :: Parser (Maybe (Exp -> Exp))
parseExp' = do
          (lexeme.char) '+'
          e1 <- parseMul
          e2 <- parseExp'
          case e2 of 
               Nothing -> return (Just (\e  -> Plus e e1))
               Just e  -> return (Just (\e' -> e (Plus e' e1)))
          <|>
            do
          (lexeme.char) '-'
          e1 <- parseMul
          e2 <- parseExp'
          case e2 of 
               Nothing -> return (Just (\e  -> Minu e e1))
               Just e  -> return (Just (\e' -> e (Minu e' e1)))
          <|>
          return Nothing

parseMul :: Parser Exp
parseMul = do
        e1 <- parseUExp
        e2 <- parseMul'
        case e2 of 
            Nothing -> return e1
            Just e  -> return (e e1)
            
parseMul' :: Parsec String () (Maybe (Exp -> Exp))
parseMul' = do
          (lexeme.char) '*'
          e1 <- parseUExp
          e2 <- parseMul'
          case e2 of
               Nothing -> return (Just (\e -> Mult e e1))
               Just  e -> return (Just (\e'-> e (Mult e' e1)))
          <|>
            do
          (lexeme.char) '/'
          e1 <- parseUExp
          e2 <- parseMul'
          case e2 of
               Nothing -> return (Just (\e -> Divi e e1))
               Just  e -> return (Just (\e'-> e (Divi e' e1)))
          <|>  
          return Nothing

parseUExp :: Parser Exp
parseUExp = do
        op <- choice (map (try.lexeme.string) 
 		     ["-","+","log","ln","sin","sqrt","cos"])
        e1 <- parseUExp
        case op of
            "-" -> return $ Nega e1
            "+" -> return $ Posi e1
            "sqrt" -> return $ Sqrt e1           
            "log" -> return $ Log e1
            "ln" -> return $ Ln e1
            "sin" -> return $ Sin e1
            "cos" -> return $ Cos e1
            _ -> fail "expect an unary operator"
        <|> parsePower 


parsePower :: Parser Exp
parsePower = try (do
        num <- parseNum
        lexeme $ char '^'
        e1 <- parsePower
        return (Power num e1))
        <|> parseNum

parseNum :: Parser Exp
parseNum = try (do
        lexeme $ char '('
        e1 <- parseExp
        lexeme $ char ')'
        return e1) <|> 
          do 
        num<- number
        return (Val num)
