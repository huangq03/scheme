module Parsers.StringParser
( parseString
)
where
import Text.Parsec hiding (spaces)
import Text.Parsec.String
import Parsers.LispVal

parseString :: Parser LispVal
parseString = do
                _ <- char '"'
                x <- many (noneOf "\"")
                _ <- char '"'
                return $ String x
