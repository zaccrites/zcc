
module Compiler.Lexer.Keyword (
  Keyword (..),
  keywordToString
)
where


data Keyword
  = KeywordBreak
  | KeywordContinue
  | KeywordDo
  | KeywordElse
  | KeywordFor
  | KeywordGoto
  | KeywordIf
  | KeywordInt
  | KeywordReturn
  | KeywordVoid
  | KeywordWhile
  deriving (Show, Eq)


keywordToString :: Keyword -> String
keywordToString keyword = case keyword of
  KeywordBreak -> "break"
  KeywordContinue -> "continue"
  KeywordDo -> "do"
  KeywordElse -> "else"
  KeywordFor -> "for"
  KeywordGoto -> "goto"
  KeywordIf -> "if"
  KeywordInt -> "int"
  KeywordReturn -> "return"
  KeywordVoid -> "void"
  KeywordWhile -> "while"

