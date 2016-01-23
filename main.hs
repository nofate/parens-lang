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

instance Show LispVal where
  show = showVal

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

showVal :: LispVal -> String
showVal (Atom a) = a
showVal (List xs) = "(" ++ unwordsList xs ++ ")"
showVal (DottedList xs x) = "(" ++ unwordsList xs ++ "." ++ showVal x ++ ")"
showVal (Number n) = show n
showVal (String s) = "\"" ++ s ++ "\""
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (Char c) = "#\\" ++ [c]

-- Parsing

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

-- Evaluation

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum (String n) = 
  let parsed = reads n :: [(Integer, String)] in
    if null parsed
    then 0
    else fst $ parsed !! 0  
unpackNum (List [n]) = unpackNum n
unpackNum _ = 0

primitives :: [(String, [LispVal] -> LispVal)]
primitives = [("+", numericBinop (+)),
              ("*", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem)]

apply :: String -> [LispVal] -> LispVal
apply func args = 
  maybe (Bool False) ($ args) $ lookup func primitives

eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval val@(Char _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args)) = apply func $ map eval args


-- Input

readExpr :: String -> LispVal
readExpr input =
  case parse parseExpr "source" input of
    Left err -> String $ "No match: " ++ show err
    Right val -> val 


main :: IO ()
main = do
  expr <- getLine
  print $ eval $ readExpr expr

