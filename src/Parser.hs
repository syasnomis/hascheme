module Parser
    (
      readExpr
    ) where
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import Numeric (readDec, readHex, readOct, readFloat)

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             | Character Char
             | Float Float 
             deriving (Show)

nonEscape :: Parser Char
nonEscape = noneOf "\\\"\n\r\t"

escape :: Parser Char 
escape = do
          char '\\'
          q  <- oneOf "\\\"nrt"
          return $ case q of
                      'n'-> '\n'
                      'r'-> '\r'
                      't'-> '\t'
                      _  -> q


parseString :: Parser LispVal
parseString = do
                char '"'
                x <- many  (escape <|> nonEscape)
                char '"'
                return $ String x

parseChar :: Parser LispVal
parseChar = try $ do
              char '#'
              char '\\'
              (q:rest) <- many (letter <|> symbol <|> oneOf "()")
              return . Character $ case (q:rest) of
                          "space" -> ' '
                          "newline" -> '\n'
                          _ -> q

parseAtom :: Parser LispVal
parseAtom = do
              first <- letter <|> symbol
              rest <- many (letter <|> digit <|> symbol)
              let atom = first:rest
              return $ case atom of
                          "#t" -> Bool True
                          "#f" -> Bool False
                          _    -> Atom atom

parseBool :: Parser LispVal
parseBool = try $ do
              char '#'
              q <- oneOf "tf"
              return $ case q of
                          't' -> Bool True
                          'f' -> Bool False
               
parseDecimal :: Parser LispVal
parseDecimal = liftM (Number . read) $ many1 digit

-- Alternative way of parsing decimal using explicit bind function
parseDecimal2 :: Parser LispVal
parseDecimal2 = (many1 digit) >>= \x -> (return . Number . read) x

-- Parse special number prefix syntax such as #o, #d, #x for octal, hex, etc.
parseNumPrefix :: Parser LispVal 
parseNumPrefix = try $ do
                  char '#'
                  q <- oneOf "odx"
                  liftM (Number . fst . head) $ case q of
                     'o' -> liftM readOct $ many1 digit
                     'd' -> liftM readDec $ many1 digit
                     'x' -> liftM readHex $ many1 (letter <|> digit)

parseFloat :: Parser LispVal
parseFloat = try $ do
              whole <- many1 digit
              char '.'
              frac <- many1 digit
              return $ (Float . fst . head) $ readFloat (whole ++ "." ++ frac) 

-- Combine numeric parsing functions into one call
parseNumber :: Parser LispVal
parseNumber = parseFloat <|> parseNumPrefix <|> parseDecimal

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

parseExpr :: Parser LispVal
parseExpr =  parseBool
         <|> parseChar
         <|> parseAtom
         <|> parseString
         <|> parseNumber
         <|> parseQuoted
         <|> do char '('
                x <- try parseList <|> parseDottedList
                char ')'
                return x

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
  Left err -> "No match: " ++ show err
  Right val -> "Found value " ++ show val
