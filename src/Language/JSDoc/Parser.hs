module Language.JSDoc.Parser where

import Language.JSDoc.Parser.Parser
import qualified Text.ParserCombinators.Parsec as Parsec 

parse :: String -> Either Parsec.ParseError JSDocSignature
parse input = Parsec.parse jsdoc "" input