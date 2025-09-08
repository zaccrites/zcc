
module Compiler.Lexer.Token (
  Token (..),
  tokenToString
)
where

import Compiler.Lexer.Keyword


data Token
  = Identifier String
  | Constant Integer
  | Keyword Keyword
  | OpenParen
  | CloseParen
  | OpenBrace
  | CloseBrace
  | Plus
  | Minus
  | Decrement
  | Tilde
  | Semicolon
  | EndOfFile
  deriving (Show, Eq)


tokenToString :: Token -> String
tokenToString token  = case token of
  Identifier name -> name
  Constant value -> show value
  Keyword keyword -> keywordToString keyword
  OpenParen -> "("
  CloseParen -> ")"
  OpenBrace -> "{"
  CloseBrace -> "}"
  Semicolon -> ";"
  Plus -> "+"
  Minus -> "-"
  Decrement -> "--"
  Tilde -> "~"
  EndOfFile -> "<EOF>"

