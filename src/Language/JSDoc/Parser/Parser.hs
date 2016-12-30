module Language.JSDoc.Parser.Parser where

import Text.ParserCombinators.Parsec
import Control.Applicative hiding ((<|>), optional, many)
import Language.JSDoc.Parser.Base

type Name = [Char]
type TypeName = [Char]
type Param = (Name, TypeName)

data JSDocSignature =
    JSDocSignature
          {   paramsType :: [Param]
          ,   returnType :: [Char]
          ,   private :: Bool
          ,   description :: Maybe [Char]
          } deriving Show

startJSDoc :: Parser [Char]
startJSDoc = lexeme (string "/**")

endJSDoc :: Parser [Char]
endJSDoc = lexeme (string "*/")

jsdoc :: Parser JSDocSignature
jsdoc = between (startJSDoc <* eol) endJSDoc parseBody

parseBody :: Parser JSDocSignature
parseBody = do
    manyTill commentString (try $ lookAhead param)
    params <- manyTill param (startReturnLine <|> privateLine <|> endOfJSDoc)
    returnTy <- option "void" (try returnT)
    private <- option False (try $ parsePrivate)
    manyTill anyChar endOfJSDoc
    return $ JSDocSignature params returnTy private Nothing
    where
        startReturnLine = try $ lookAhead (lexeme $ string "* @r")
        endOfJSDoc = try $ lookAhead $ lexeme $ string "*/"
        privateLine = try $ lookAhead (string " * @private")

initCommentLine :: Parser Char
initCommentLine = lexeme $ char '*'

commentString :: Parser String
commentString =  initCommentLine *> many (noneOf ['\r', '\n']) <* eol

param :: Parser Param
param = (pure genParam) <* initCommentLine <* lexeme (string "@param") <*> typeName <*> variableName <* paramDescription

typeName :: Parser [Char]
typeName =  char '{' *> many (noneOf ['}']) <* char '}'

variableName :: Parser String
variableName = lexeme (many (noneOf [' ', ':', '\n']))

paramDescription :: Parser String
paramDescription = combineDescription <$> lexeme (many (noneOf ['\n'])) <* string "\n" <*> manyTill commentString (startNextParam <|> startReturnLine <|> endOfJSDoc)
    where
          startNextParam = try $ lookAhead (lexeme $ string "* @p")
          startReturnLine = try $ lookAhead (lexeme $ string "* @r")
          endOfJSDoc = try $ lookAhead (lexeme $ string "*/")

combineDescription :: String -> [String] -> String
combineDescription s descs = s ++ (foldl (++) "" descs)

genParam :: String -> String -> Param
genParam typeName varName = (varName, typeName)

returnT :: Parser [Char]
returnT = do
     many $ noneOf ['@']
     lexeme (syntacticSugar "@return")
     typeN <- typeName
     descr <- lexeme (many (noneOf ['\n']))
     string "\n"
     return typeN

startJsDoc :: Parser [Char]
startJsDoc = lexeme (syntacticSugar "/**") *> string "\n"

parsePrivate :: Parser Bool
parsePrivate = string " * @private\n" >> return True
