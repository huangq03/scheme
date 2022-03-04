module Lib
    ( someFunc
    , symbol
    ) where
import Text.Parsec hiding (spaces)
import Text.Parsec.String
import System.Environment
import Control.Monad

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             deriving Show

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

--spaces :: Parser ()
--spaces = skipMany1 space

parseString :: Parser LispVal
parseString = do
                _ <- char '"'
                x <- many (noneOf "\"")
                _ <- char '"'
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

parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

parseExpr :: Parser LispVal
parseExpr = parseAtom
         <|> parseString
         <|> parseNumber

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
  Left err -> "No match: " ++ show err
  Right val -> "Found value " ++ show val

someFunc :: IO ()
someFunc = do
  (expr:_) <- getArgs
  putStrLn (readExpr expr)
