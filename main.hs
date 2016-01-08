import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad

-- data ParsecT s u m a
-- s - stream type
-- u - user state type
-- m - underlying monad 
-- a - return type
--
-- type Parsec s u = ParsecT s u Identity

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             deriving Show

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space


parseStringQuote :: Parser Char
parseStringQuote = do 
  char '\\'
  char '\"'
  return '\"'

parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many (parseStringQuote <|> noneOf "\"")
  char '"'
  return $ String x

parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit


parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first:rest
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    _    -> Atom atom

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> parseNumber


readExpr :: String -> String
readExpr input =
  case parse parseExpr "source" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value: " ++ show val 


main :: IO ()
main = do
  expr <- getLine
  putStrLn $ "Source: " ++ expr
  putStrLn $ readExpr expr

