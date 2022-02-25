module Lib
    ( someFunc
    , symbol
    ) where
import Text.Parsec hiding (spaces)
import Text.Parsec.String
import System.Environment

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

readExpr :: String -> String
readExpr input = case parse (spaces >> symbol) "lisp" input of
  Left err -> "No match: " ++ show err
  Right val -> "Found value " ++ show val

someFunc :: IO ()
someFunc = do
  (expr:_) <- getArgs
  putStrLn (readExpr expr)
