
module Compiler.Lexer.Keyword (
  Keyword (..),
  keywordToString
)
where


data Keyword
  = KeywordInt
  | KeywordVoid
  | KeywordReturn
  deriving (Show, Eq)


keywordToString :: Keyword -> String
keywordToString keyword = case keyword of
  KeywordInt -> "int"
  KeywordVoid -> "void"
  KeywordReturn -> "return"

