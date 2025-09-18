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
    "break" -> Keyword KeywordBreak
    "case" -> Keyword KeywordCase
    "continue" -> Keyword KeywordContinue
    "default" -> Keyword KeywordDefault
    "do" -> Keyword KeywordDo
    "else" -> Keyword KeywordElse
    "for" -> Keyword KeywordFor
    "goto" -> Keyword KeywordGoto
    "if" -> Keyword KeywordIf
    "int" -> Keyword KeywordInt
    "return" -> Keyword KeywordReturn
    "switch" -> Keyword KeywordSwitch
    "void" -> Keyword KeywordVoid
    "while" -> Keyword KeywordWhile
    xs -> Identifier xs


isIdentifierHeadChar :: Char -> Bool
isIdentifierHeadChar c = isAlpha c || '_' == c

isIdentifierChar :: Char -> Bool
isIdentifierChar c = isIdentifierHeadChar c || isDigit c

