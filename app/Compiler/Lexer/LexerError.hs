{-# LANGUAGE  NoFieldSelectors #-}
{-# LANGUAGE RecordWildCards #-}

module Compiler.Lexer.LexerError (
  LexerError (..)
) where

import Compiler.SourceLocation (SourceLocation (..))

data LexerError = LexerError {
  message :: String,
  location :: SourceLocation
}

instance Show LexerError where
  show (LexerError {..}) =
    let
      SourceLocation line column = location
      parts = [show line, ":", show column, " : ", message]
    in concat parts

