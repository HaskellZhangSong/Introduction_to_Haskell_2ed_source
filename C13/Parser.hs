-- Parser.hs
import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Data.Char

data Node = Tag String [Node]
          | Text String
           deriving (Show,Eq)

xml :: String
xml = "<html><head>Hello world!</head><body>Hello again!</body></html>"

type Parser a = StateT String Maybe a

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = StateT $ \str -> case str of 
                               [] -> Nothing
                               s:ss -> if p s then Just (s,ss) else Nothing
char :: Char -> Parser Char
char c = satisfy (==c) 

letter = satisfy isAlpha
string str = mapM char str


runParser :: Parser a -> String -> Maybe (a, String)
runParser = runStateT

textNode :: Parser Node
textNode = fmap Text $ some $ satisfy (/='<')

tagNode :: Parser Node
tagNode = do
        tagName <- char '<' *> many letter <* char '>'
        subNode <- many $ tagNode <|> textNode
        string "</" >> string tagName >> char '>'
        return $ Tag tagName subNode
