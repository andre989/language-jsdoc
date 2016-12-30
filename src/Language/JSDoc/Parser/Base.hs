module Language.JSDoc.Parser.Base where

import Text.ParserCombinators.Parsec
import Control.Applicative hiding ((<|>), optional, many)
import Debug.Trace

ws :: Parser String
ws = many (oneOf " ")

int :: (Integral a, Read a) => Parser a
int = read <$> many1 digit

stringLike :: Parser String
stringLike = char '"' *> many (noneOf ['\"', '\r', '\n']) <* char '"'

lexeme :: Parser a -> Parser a
lexeme p = ws *> p <* ws

syntacticSugar :: String -> Parser (Maybe String)
syntacticSugar s = (string s *> (pure . Just $ s)) <|> pure Nothing

eol :: Parser String
eol = (try $ string "\n") <|> (try $ string "\r\n") <|> string "\r"

emptyLine :: Parser String
emptyLine = ws *> eol

seeNext :: Int -> Parser ()
seeNext n = do
  s <- getParserState
  let out = take n (stateInput s)
  println out

println msg = trace (show msg) $ return ()