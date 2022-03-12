module Lib
    ( someFunc
    ) where
import Text.Parsec hiding (spaces)
import System.Environment

import Parsers.LispVal
import Parsers.LispError
import Control.Monad.Except
import Parsers.MainParsers

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left err -> throwError $ Parser err
  Right val -> return val

someFunc :: IO ()
someFunc = do
  args <- getArgs
  evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
  putStrLn $ extractValue $ trapError evaled
