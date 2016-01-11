import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Numeric

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
             | Char Char
             deriving Show

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space


parseStringEscape :: Parser Char
parseStringEscape = do 
  char '\\'
  c <- oneOf "\"\\trn"
  case c of
    't'  -> return '\t'
    'r'  -> return '\r'
    'n'  -> return '\n' 
    x    -> return x
  
parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many (parseStringEscape <|> noneOf "\"")
  char '"'
  return $ String x

parseDecNumber :: Parser LispVal
parseDecNumber = liftM (Number . read) $ many1 digit

parseHexNumber :: Parser LispVal
parseHexNumber = liftM (Number . fst. head . readHex) $ many1 hexDigit

parseOctNumber :: Parser LispVal
parseOctNumber = liftM (Number . fst. head . readOct) $ many1 octDigit


parseAtomSimple :: [Char] -> Parser LispVal
parseAtomSimple h = do
  rest <- many (letter <|> digit <|> symbol)
  let atom = h ++ rest
  return $ Atom atom

parseAtomExt :: Parser LispVal
parseAtomExt = do
  c <- letter <|> digit <|> symbol 
  case c of
    't' -> return $ Bool True
    'f' -> return $ Bool False  
    'd' -> parseDecNumber
    'x' -> parseHexNumber
    'o' -> parseOctNumber
    x   -> parseAtomSimple ['#', x]

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  if first == '#' then
    parseAtomExt 
  else   
    parseAtomSimple [first]

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
  x <- endBy parseExpr spaces
  xs <- char '.' >> spaces >> parseExpr
  return $ DottedList x xs

parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> parseDecNumber
        <|> parseQuoted
        <|> do char '('
               x <- try parseList <|> parseDottedList
               char ')'
               return x 

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

