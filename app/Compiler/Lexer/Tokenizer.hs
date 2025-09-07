{-# LANGUAGE  OverloadedRecordDot #-}

module Compiler.Lexer.Tokenizer (
  getTextTokens
)
where

import Compiler.Lexer.LexerError
import Compiler.Lexer.Token
import Compiler.Lexer.Lexer
import Compiler.Lexer.LexerState
import Compiler.Lexer.IdentifierLexing
import Compiler.Lexer.IntLexing
import Compiler.Lexer.SourceToken

import Data.Char
import Control.Monad.State (runState, get)


getTextTokens :: String -> ([SourceToken], [LexerError])
getTextTokens text =
  let (tokens, endState) = runState getTextTokens' (newLexerState text)
  in (tokens, endState.errors)



getTextTokens' :: Lexer [SourceToken]
getTextTokens' = do
  token <- getNextToken
  case token of
    Nothing -> return []
    Just t@(SourceToken EndOfFile _) -> return [t]
    Just x -> (x :) <$> getTextTokens'


getNextToken :: Lexer (Maybe SourceToken)
getNextToken = do
  consumeSpace
  nextChar <- peekNextChar
  case nextChar of
    Nothing -> give EndOfFile
    Just x -> case x of
      '(' -> give' OpenParen
      ')' -> give' CloseParen
      '{' -> give' OpenBrace
      '}' -> give' CloseBrace
      ';' -> give' Semicolon
      '+' -> give' Plus
      '-' -> give' Minus
      c | isDigit c -> readIntLiteralToken >>= maybeGive
      c | isIdentifierHeadChar c -> readIdentifierOrKeyword >>= give
      c -> emitUnexpectedCharError c >> return Nothing

  where

    withLoc :: Token -> Lexer SourceToken
    withLoc token = do
      current <- get
      return $ SourceToken token current.location

    give :: Token -> Lexer (Maybe SourceToken)
    give token = do
      result <- withLoc token
      return $ Just result

    give' :: Token -> Lexer (Maybe SourceToken)
    give' token = tossNextChar >> give token

    maybeGive :: Maybe Token -> Lexer (Maybe SourceToken)
    maybeGive token = do
      case token of
        Nothing -> return Nothing
        Just token' -> do
          result <- withLoc token'
          return $ Just result

    emitUnexpectedCharError :: Char -> Lexer ()
    emitUnexpectedCharError c = do
      let msg = "unexpected '" ++ (c:"'")
      emitLexerError msg

