module Main where
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
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
              
parseNumber1 :: Parser LispVal
parseNumber1 = liftM (Number . read) $ many1 digit

parseNumber2 :: Parser LispVal
parseNumber2 = (many1 digit) >>= \x -> (return . Number . read) x

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

parseNumber :: Parser LispVal
parseNumber = parseFloat <|> parseNumPrefix <|> parseNumber1

parseExpr :: Parser LispVal
parseExpr =  parseBool
         <|> parseChar
         <|> parseAtom
         <|> parseString
         <|> parseNumber

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
  Left err -> "No match: " ++ show err
  Right val -> "Found value " ++ show val

main :: IO ()
main = do
  (expr:rest) <- getArgs
  putStrLn expr
  putStrLn $ show rest
  putStrLn (readExpr expr) 

