{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DisambiguateRecordFields #-}

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
import Control.Monad (when)


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


consumeSpacesAndComments :: Lexer ()
consumeSpacesAndComments = do
  _ <- consumeSpace
  hasComment <- consumeComment
  when hasComment consumeSpacesAndComments


getNextToken :: Lexer (Maybe SourceToken)
getNextToken = do
  consumeSpacesAndComments
  LexerState { location=location } <- get

  nextChar <- peekNextChar
  case nextChar of
    Nothing -> give EndOfFile
    Just x -> case x of
      '(' -> give' OpenParen
      ')' -> give' CloseParen
      '{' -> give' OpenBrace
      '}' -> give' CloseBrace
      ';' -> give' Semicolon
      '~' -> give' Tilde
      '?' -> give' QuestionMark
      ':' -> give' Colon
      '+' -> tossNextChar >> readPlusToken >>= give
      '-' -> tossNextChar >> readMinusToken >>= give
      '*' -> tossNextChar >> ifNextCharMatches '=' AsteriskAssign Asterisk >>= give
      '/' -> tossNextChar >> ifNextCharMatches '=' SlashAssign Slash >>= give
      '%' -> tossNextChar >> ifNextCharMatches '=' PercentAssign Percent >>= give
      '^' -> tossNextChar >> ifNextCharMatches '=' CaretAssign Caret >>= give
      '|' -> tossNextChar >> ifNextCharMatches '|' DoublePipe Pipe >>= give
      '&' -> tossNextChar >> ifNextCharMatches '&' DoubleAmpersand Ampersand >>= give
      '<' -> tossNextChar >> readLessThanToken >>= give
      '>' -> tossNextChar >> readGreaterThanToken >>= give
      '=' -> tossNextChar >> ifNextCharMatches '=' Equal Assign >>= give
      '!' -> tossNextChar >> ifNextCharMatches '=' NotEqual Exclamation >>= give
      c | isDigit c -> readIntLiteralToken >>= maybeGive
      c | isIdentifierHeadChar c -> readIdentifierOrKeyword >>= give
      c -> emitUnexpectedCharError c >> return Nothing

  where

    -- TODO: fix the "off-by-one" error here, where the reported location
    -- of the token will not be correct. It will point to the end of the token,
    -- not the start of it, because withLoc happens after the position
    -- has been moved up.
    --
    -- Probably it can be solved by just giving a SourceLocation
    -- argument to the "give" function, rather than getting the current
    -- location from withLoc

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


ifNextCharMatches :: Char -> Token -> Token -> Lexer Token
ifNextCharMatches c ifYes ifNo = do
  nextChar <- peekNextChar
  case nextChar of
    Just c' | c == c' -> tossNextChar >> return ifYes
    _ -> return ifNo


readPlusToken :: Lexer Token
readPlusToken = do
  nextChar <- peekNextChar
  case nextChar of
    Just '+' -> tossNextChar >> return Increment
    Just '=' -> tossNextChar >> return PlusAssign
    _ -> return Plus


readMinusToken :: Lexer Token
readMinusToken = do
  nextChar <- peekNextChar
  case nextChar of
    Just '-' -> tossNextChar >> return Decrement
    Just '=' -> tossNextChar >> return MinusAssign
    _ -> return Minus


readLessThanToken :: Lexer Token
readLessThanToken = do
  nextChar <- peekNextChar
  case nextChar of
    Just '<' -> tossNextChar >> ifNextCharMatches '=' ShiftLeftAssign ShiftLeft
    Just '=' -> tossNextChar >> return LessThanEqual
    _ -> return LessThan


readGreaterThanToken :: Lexer Token
readGreaterThanToken = do
  nextChar <- peekNextChar
  case nextChar of
    Just '>' -> tossNextChar >> ifNextCharMatches '=' ShiftRightAssign ShiftRight
    Just '=' -> tossNextChar >> return GreaterThanEqual
    _ -> return GreaterThan

