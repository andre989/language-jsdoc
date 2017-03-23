module Language.JSDoc.Parser.Parser where

import           Control.Applicative           hiding (many, optional, (<|>))
import           Language.JSDoc.Parser.Base
import           Text.ParserCombinators.Parsec

input = unlines
    [   "/**"
    ,   " * Substitute %n parameters in a string"
    ,   " * @param {String} string The source string to substitue %n occurences in"
    ,   " * @param {Array|String} params The parameters to use for the substitution. Index 0 will replace %1, index 1, %2"
    ,   " * and so on. If a string is passed, only %1 will be replaced"
    ,   " * @return {String} The final string, with %n occurences replaced with their equivalent"
    ,   " */"
    ]
data JSDocAst =
    JSDocAst
        {   jsdocDescription :: String
        ,   tags             :: [Tag]
        } deriving Show

data Tag =
    ParamTag TypeAnnotation String String |
    ReturnTag TypeAnnotation |
    PrivateTag deriving Show

data TypeAnnotation =
    NameExpression String |
    ParametricType String [TypeAnnotation] |
    NonNullableType TypeAnnotation deriving Show

startJSDoc :: Parser String
startJSDoc = lexeme (string "/**")

endJSDoc :: Parser String
endJSDoc = lexeme (string "*/")

jsdoc :: Parser JSDocAst
jsdoc = between (startJSDoc <* eol) endJSDoc parseBody

parseBody :: Parser JSDocAst
parseBody = JSDocAst <$> paramDescription <*> many parseTag

parseTag :: Parser Tag
parseTag = initTagLine *>
    seeNext 10 *>
    try (lookAhead parseParamTag) <|>
    try (lookAhead parseReturnTag) <|>
    try (lookAhead parsePrivateTag) <?>
    "Error"

initTagLine :: Parser String
initTagLine = initCommentLine *> string "@"

parseParamTag :: Parser Tag
parseParamTag =
    pure ParamTag
    <*  string "param"
    <*>  between (lexeme $ string "{") (lexeme $ string "}") parseTypeAnnotation
    <*> variableName
    <*> paramDescription

parseReturnTag :: Parser Tag
parseReturnTag = pure ReturnTag <* string "return" <*> parseTypeAnnotation

parsePrivateTag :: Parser Tag
parsePrivateTag = pure PrivateTag <* string "private"

parseTypeAnnotation :: Parser TypeAnnotation
parseTypeAnnotation =
    NameExpression <$> many (noneOf ['}'])

initCommentLine :: Parser Char
initCommentLine = lexeme $ char '*'

commentString :: Parser String
commentString =  initCommentLine *> many (noneOf ['\r', '\n']) <* eol


variableName :: Parser String
variableName = lexeme (many (noneOf [' ', ':', '\n']))

paramDescription :: Parser String
paramDescription = combineDescription
    <$> lexeme (many (noneOf ['\n']))
    <* string "\n"
    <*> manyTill commentString (try $ lookAhead initTagLine <|> endOfJSDoc)
    where
        endOfJSDoc = try $ lookAhead (lexeme $ string "*/")

combineDescription :: String -> [String] -> String
combineDescription s descs = s ++ concat descs


startJsDoc :: Parser String
startJsDoc = lexeme (syntacticSugar "/**") *> string "\n"
