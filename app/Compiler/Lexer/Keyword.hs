
module Compiler.Lexer.Keyword (
  Keyword (..),
  keywordToString
)
where


data Keyword
  = KeywordBreak
  | KeywordCase
  | KeywordContinue
  | KeywordDefault
  | KeywordDo
  | KeywordElse
  | KeywordFor
  | KeywordGoto
  | KeywordIf
  | KeywordInt
  | KeywordReturn
  | KeywordSwitch
  | KeywordVoid
  | KeywordWhile
  deriving (Show, Eq)


keywordToString :: Keyword -> String
keywordToString keyword = case keyword of
  KeywordBreak -> "break"
  KeywordCase -> "case"
  KeywordContinue -> "continue"
  KeywordDefault -> "default"
  KeywordDo -> "do"
  KeywordElse -> "else"
  KeywordFor -> "for"
  KeywordGoto -> "goto"
  KeywordIf -> "if"
  KeywordInt -> "int"
  KeywordReturn -> "return"
  KeywordSwitch -> "switch"
  KeywordVoid -> "void"
  KeywordWhile -> "while"

