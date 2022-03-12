module Parsers.NumberParser
( parseNumber
)
where
import Text.Parsec hiding (spaces)
import Text.Parsec.String
import Control.Monad
import Parsers.LispVal

parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit
