module Parsers.AtomParser
( parseAtom
)
where
import Text.Parsec hiding (spaces)
import Text.Parsec.String
import Parsers.LispVal
import Parsers.MiscParsers

parseAtom :: Parser LispVal
parseAtom = do
              first <- letter <|> symbol
              rest <- many (letter <|> digit <|> symbol)
              let atom = first:rest
              return $ case atom of
                        "#t" -> Bool True
                        "#f" -> Bool False
                        _    -> Atom atom
