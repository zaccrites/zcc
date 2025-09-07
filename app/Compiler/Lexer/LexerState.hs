{-# LANGUAGE NoFieldSelectors #-}

module Compiler.Lexer.LexerState (
  LexerState (..),
  newLexerState
)
where

import Compiler.Lexer.LexerError (LexerError)
import Compiler.SourceLocation

data LexerState = LexerState {
  errors :: [LexerError],
  location :: SourceLocation,
  text :: String
} deriving(Show)

newLexerState :: String -> LexerState
newLexerState text = LexerState {
  errors = [],
  location = firstLocation,
  text = text
}

