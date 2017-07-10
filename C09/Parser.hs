-- Parser.hs
{-# LANGUAGE DeriveFunctor #-}
import Control.Applicative
import Data.Char

newtype Parser a = Parser { runParser :: String -> Maybe (a,String) }
                                deriving Functor

instance Applicative Parser where
    pure a = Parser $ \str -> Just (a,str)
    (<*>) fp a = Parser $ \str -> 
                case runParser fp str of
                    Nothing -> Nothing
                    Just (ab,s) -> case runParser a s of
                                       Nothing -> Nothing
                                       Just (at,s1) -> Just (ab at,s1)

instance Alternative Parser where
        empty = Parser $ \_ -> Nothing
        (<|>) a b = Parser $ \str -> case runParser a str  of
                                         Nothing -> runParser b str
                                         just       -> just

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser $ \str -> case str of 
                                 [] -> Nothing
                                 s:ss -> if f s then Just (s,ss) else Nothing

char :: Char -> Parser Char
char c = satisfy (==c)

number :: Parser Int
number = fmap (foldl (\x y -> 10*x+y) 0) (many digit)
      where digit = fmap digitToInt (satisfy isDigit)
      
sequ :: Parser a -> Parser [a] -> Parser [a]
sequ x y = Parser $ \str -> case runParser x str of
                              Nothing -> Nothing
                              Just (s,ss) -> case runParser y ss of
                                               Nothing -> Nothing
                                               Just (s1,ss1) -> Just (s:s1,ss1) 

parseStr :: [Char] -> Parser [Char]
parseStr strs = foldr sequ (Parser $ \str -> Just ("",str)) [char s| s <- strs]
