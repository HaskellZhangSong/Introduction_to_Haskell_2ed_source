-- ExprQuote.hs
{-# LANGUAGE DeriveDataTypeable #-}
module ExprQuote where
import Text.Parsec
import Text.Parsec.String
import Data.Char(digitToInt, isDigit, isAlpha)
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Data.Generics

data Expr = Val Int
          | Id String
          | Add Expr Expr
            deriving (Show, Eq, Data)

parseAdd :: Parser (Expr -> Expr -> Expr)
parseAdd = do
        char '+'
        return Add

parseInt :: Parser Expr
parseInt = fmap Val $ fmap (foldl (\x y -> 10*x + y) 0) (many1 digit)
                            where digit = fmap digitToInt (satisfy isDigit)

parseId :: Parser Expr
parseId = fmap Id $ many1 (satisfy isAlpha)

parseExpr' :: Parser Expr
parseExpr' = foldl1 (<|>) [parseInt, parseId, char '(' *>  parseExpr <*  char ')']

parseExpr :: Parser Expr
parseExpr = chainl1 parseExpr' parseAdd

generalized_parseExpr :: Monad m => String -> m Expr
generalized_parseExpr s = case runParser parseExpr () "" s of
                             Left err -> error $ show err
                             Right e -> return e

quoteExprExp :: String -> ExpQ
quoteExprExp s = do
        exp <- generalized_parseExpr s
        dataToExpQ (const Nothing) exp
{-
quoteExprPat :: String -> Q Pat
quoteExprPat s = do
        exp <- generalized_parseExpr s
        dataToPatQ (const Nothing) exp
-}
expr  :: QuasiQuoter
expr  =  QuasiQuoter { quoteExp = quoteExprExp,
                       quotePat = quoteExprPat,
                       quoteDec = undefined,
                       quoteType = undefined }

antiExprPat :: Expr -> Maybe (Q Pat)
antiExprPat (Id v) = Just $ varP (mkName v)
antiExprPat _ = Nothing

quoteExprPat :: String -> Q Pat
quoteExprPat s = do
  exp <- generalized_parseExpr s
  dataToPatQ (const Nothing `extQ` antiExprPat) exp