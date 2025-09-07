
module Compiler.Lexer.SourceToken (
  SourceToken (..),
)
where

import Compiler.Lexer.Token
import Compiler.SourceLocation


data SourceToken = SourceToken Token SourceLocation
  deriving (Show)

