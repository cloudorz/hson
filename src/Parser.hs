module Parser where

import Control.Monad (void)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

rword :: String -> Parser ()
rword w = (lexeme . try) (string w *> notFollowedBy alphaNumChar)

jsonString :: Parser String
jsonString = between (char '"') (char '"') (many (try alphaNumChar <|> spaceChar))

jString :: Parser String
jString = lexeme jsonString

jNumber = lexeme L.decimal

jBool :: Parser Bool
jBool = (return True <* rword "true") <|> (return False <* rword "false")

colon :: Parser ()
colon = void $ symbol ":"

comma :: Parser ()
comma = void $ symbol ","
