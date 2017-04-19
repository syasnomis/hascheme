module Main where
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

nonQuote :: Parser Char
nonQuote = noneOf "\\\""

quote :: Parser Char 
quote = do
          char '\\'
          q  <- oneOf "\\\""
          return q


parseString :: Parser LispVal
parseString = do
                char '"'
                x <- many  (quote <|> nonQuote)
                char '"'
                return $ String x

parseAtom :: Parser LispVal
parseAtom = do
              first <- letter <|> symbol
              rest <- many (letter <|> digit <|> symbol)
              let atom = first:rest
              return $ case atom of
                          "#t" -> Bool True
                          "#f" -> Bool False
                          _    -> Atom atom

parseNumber1 :: Parser LispVal
parseNumber1 = liftM (Number . read) $ many1 digit

parseNumber2 :: Parser LispVal
parseNumber2 = (many1 digit) >>= \x -> (return . Number . read) x

parseNumber :: Parser LispVal
parseNumber = do
                num <- many1 digit
                (return . Number . read) num

parseExpr :: Parser LispVal
parseExpr = parseAtom
         <|> parseString
         <|> parseNumber

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
  Left err -> "No match: " ++ show err
  Right val -> "Found value"

main :: IO ()
main = do
  (expr:_) <- getArgs
  putStrLn (readExpr expr) 

