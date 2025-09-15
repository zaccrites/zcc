
module Compiler.Lexer.Keyword (
  Keyword (..),
  keywordToString
)
where


data Keyword
  = KeywordInt
  | KeywordVoid
  | KeywordReturn
  | KeywordIf
  | KeywordElse
  | KeywordGoto
  deriving (Show, Eq)


keywordToString :: Keyword -> String
keywordToString keyword = case keyword of
  KeywordInt -> "int"
  KeywordVoid -> "void"
  KeywordReturn -> "return"
  KeywordIf -> "if"
  KeywordElse -> "else"
  KeywordGoto -> "goto"

