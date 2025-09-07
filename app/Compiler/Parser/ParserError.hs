{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE RecordWildCards #-}

module Compiler.Parser.ParserError (
  ParserError (..),
  formatParserError,
)
where

import Compiler.Lexer.SourceToken (SourceToken (..))


data ParserError = ParserError {
  token :: SourceToken,
  message :: String
} deriving (Show)


formatParserError :: ParserError -> String
formatParserError (ParserError {..}) =
  "ERROR: [" ++ show location ++ "] " ++ message ++ " (@ " ++ show token' ++ ")"
  where
    SourceToken token' location = token

