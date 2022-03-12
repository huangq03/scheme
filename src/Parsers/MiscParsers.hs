module Parsers.MiscParsers
( symbol
, spaces
)
where
import Text.Parsec hiding (spaces)
import Text.Parsec.String

spaces :: Parser ()
spaces = skipMany1 space

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"
