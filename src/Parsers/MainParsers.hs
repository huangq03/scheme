module Parsers.MainParsers
( parseExpr
, eval
, Env
, nullEnv
)
where
import Control.Monad
import Text.Parsec.String
import Text.Parsec hiding (spaces)
import Parsers.LispVal
import Parsers.MiscParsers
import Parsers.AtomParser (parseAtom)
import Parsers.StringParser (parseString)
import Parsers.NumberParser (parseNumber)
import Parsers.Helpers (eval, nullEnv)

parseExpr :: Parser LispVal
parseExpr = parseAtom
         <|> parseString
         <|> parseNumber
         <|> parseQuoted
         <|> do _ <- char '('
                x <- try parseList <|> parseDottedList
                _ <- char ')'
                return x

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
  head' <- endBy parseExpr spaces
  tail' <- char '.' >> spaces >> parseExpr
  return $ DottedList head' tail'

parseQuoted :: Parser LispVal
parseQuoted = do
  _ <- char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]


