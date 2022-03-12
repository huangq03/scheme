module Lib
    ( someFunc
    ) where
import Text.Parsec hiding (spaces)
import System.Environment

import Parsers.LispVal
import Parsers.MainParsers

readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left err -> String $ "No match: " ++ show err
  Right val -> val

someFunc :: IO ()
someFunc = do
  getArgs >>= print . eval . readExpr . head
