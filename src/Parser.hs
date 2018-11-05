module Parser (
  Parser
, jString
, jFloat
, jInteger
, jBool
, Value(S, F, N, B, Array, Object)
, jJSON
, jArray
, jObject
) where

import Control.Monad (void)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

data Value = S String
           | F Double
           | N Integer
           | B Bool
           | Array [Value]
           | Object [(String, Value)]
           deriving (Show, Eq)

sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

checkNoFollow :: Parser a -> Parser a
checkNoFollow = (<* notFollowedBy printChar)

jString :: Parser String
jString = lexeme jsonString
  where
    anyChar = try alphaNumChar <|> spaceChar
    jsonString = between (char '"') (char '"') (many anyChar)

jFloat :: Parser Double
jFloat = L.signed sc (lexeme L.float)

jInteger :: Parser Integer
jInteger = L.signed sc (lexeme L.decimal)

jBool :: Parser Bool
jBool = (return True <* rword "true") <|> (return False <* rword "false")
  where rword = try . lexeme . string

jJSON :: Parser Value
jJSON = between sc eof jValue >>= check
  where
    check v = case v of 
                Array xs -> return $ Array xs
                Object xs -> return $ Object xs
                _ -> fail $ "json format issue"

jValue :: Parser Value
jValue = S <$> try jString
     <|> B <$> try jBool
     <|> F <$> try jFloat
     <|> N <$> try jInteger
     <|> Array <$> try jArray
     <|> Object <$> try jObject

colon :: Parser ()
colon = void $ symbol ":"

comma :: Parser ()
comma = void $ symbol ","

jArray :: Parser [Value]
jArray = between (symbol "[") (symbol "]") itemsParser
  where
    followBy = lookAhead (comma <|> void (char ']'))
    itemsParser = (jValue <* followBy) `sepBy` comma

jObject :: Parser [(String, Value)]
jObject = between (symbol "{") (symbol "}") itemsParser
  where
    followBy = lookAhead (comma <|> void (char '}'))
    keyValuePair = (,) <$> (jString <* colon) <*> jValue
    itemsParser = (keyValuePair <* followBy) `sepBy` comma
