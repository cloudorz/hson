module Parser (
  Parser
, jString
, jFloat
, jInteger
, jBool
, jKeyStringValue
, jTArray
) where

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

checkNoFollow :: Parser a -> Parser a
checkNoFollow = (<* notFollowedBy printChar)

jsonString :: Parser String
jsonString = between (char '"') (char '"') (many (try alphaNumChar <|> spaceChar))

jString :: Parser String
jString = lexeme jsonString

jFloat :: Parser Double
jFloat = checkNoFollow $ L.signed sc (lexeme L.float)

jInteger :: Parser Integer
jInteger = checkNoFollow $ L.signed sc (lexeme L.decimal)

jBool :: Parser Bool
jBool = checkNoFollow $ (return True <* rword "true") <|> (return False <* rword "false")
  where rword = try . lexeme . string

colon :: Parser ()
colon = void $ symbol ":"

comma :: Parser ()
comma = void $ symbol ","

jKeyStringValue :: Parser (String, String)
jKeyStringValue = (,) <$> (jString <* colon) <*> jString

jTArray :: Parser [String]
jTArray = jString `sepBy` comma
