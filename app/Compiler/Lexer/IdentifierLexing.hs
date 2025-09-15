module Compiler.Lexer.IdentifierLexing (
  readIdentifierOrKeyword,
  isIdentifierHeadChar,
  isIdentifierChar
)
where

import Compiler.Lexer.Lexer
import Compiler.Lexer.Token
import Compiler.Lexer.Keyword

import Data.Char


readIdentifierOrKeyword :: Lexer Token
readIdentifierOrKeyword = do
  chars <- takeCharsWhile isIdentifierChar
  return $ case chars of
    "int" -> Keyword KeywordInt
    "void" -> Keyword KeywordVoid
    "return" -> Keyword KeywordReturn
    "if" -> Keyword KeywordIf
    "else" -> Keyword KeywordElse
    "goto" -> Keyword KeywordGoto
    xs -> Identifier xs


isIdentifierHeadChar :: Char -> Bool
isIdentifierHeadChar c = isAlpha c || '_' == c

isIdentifierChar :: Char -> Bool
isIdentifierChar c = isIdentifierHeadChar c || isDigit c

