
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
  | Asterisk
  | Slash
  | Percent
  | Increment
  | Decrement
  | Tilde
  | Caret
  | Pipe
  | DoublePipe
  | Ampersand
  | DoubleAmpersand
  | LessThan
  | GreaterThan
  | LessThanEqual
  | GreaterThanEqual
  | ShiftLeft
  | ShiftRight
  | Equal
  | NotEqual
  | Exclamation
  | Assign
  | PlusAssign
  | MinusAssign
  | AsteriskAssign
  | SlashAssign
  | PercentAssign
  | AmpersandAssign
  | PipeAssign
  | CaretAssign
  | ShiftLeftAssign
  | ShiftRightAssign
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
  Asterisk -> "*"
  Slash -> "/"
  Percent -> "%"
  Increment -> "++"
  Decrement -> "--"
  Tilde -> "~"
  Caret -> "^"
  Pipe -> "|"
  Exclamation -> "!"
  DoublePipe -> "||"
  Ampersand -> "&"
  DoubleAmpersand -> "&&"
  LessThan -> "<"
  GreaterThan -> ">"
  LessThanEqual -> "<="
  GreaterThanEqual -> ">="
  ShiftLeft -> "<<"
  ShiftRight -> "<<"
  Equal -> "=="
  NotEqual -> "!="
  Assign -> "="
  PlusAssign -> "+="
  MinusAssign -> "-="
  AsteriskAssign -> "*="
  SlashAssign -> "/="
  PercentAssign -> "%="
  AmpersandAssign -> "&="
  PipeAssign -> "|="
  CaretAssign -> "^="
  ShiftLeftAssign -> "<<="
  ShiftRightAssign -> ">>="
  EndOfFile -> "<EOF>"

