{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DisambiguateRecordFields #-}

module Compiler.Lexer.Lexer (
  Lexer,
  emitLexerError,
  consumeSpace,
  takeNextChar,
  tossNextChar,
  peekNextChar,
  takeCharsWhile,
) where

import Compiler.Lexer.LexerState (LexerState(..))
import Compiler.Lexer.LexerError (LexerError(..))
import Compiler.SourceLocation (nextLineLocation, nextColumnLocation)

import Data.Char (isSpace)
import Control.Monad (void)
import Control.Monad.State (State, get, put)


type Lexer = State LexerState


emitLexerError :: String -> Lexer ()
emitLexerError message = do
  current <- get
  let err = LexerError {
    message = message,
    location = current.location
  }
  put current { errors = current.errors ++ [err] }


consumeSpace :: Lexer ()
consumeSpace = void (takeCharsWhile isSpace)


takeCharsWhile :: (Char -> Bool) -> Lexer [Char]
takeCharsWhile p = do
  nextChar <- peekNextChar
  case nextChar of
    Nothing -> return ""
    Just x -> if p x
      then do
        tossNextChar
        rest <- takeCharsWhile p
        return $ x : rest
      else return ""


peekNextChar :: Lexer (Maybe Char)
peekNextChar = do
  current <- get
  return $ case current.text of
    (x:_) -> Just x
    _ -> Nothing


takeNextChar :: Lexer (Maybe Char)
takeNextChar = do
  current <- get
  case current.text of
    "" -> return Nothing
    (x@'\n':xs) -> do
      put $ current { text=xs, location=nextLineLocation current.location }
      return $ Just x
    ('\r':xs) -> do
      put $ current { text=xs }
      takeNextChar
    (x:xs) -> do
      put $ current { text=xs, location=nextColumnLocation  current.location }
      return $ Just x


tossNextChar :: Lexer ()
tossNextChar = void takeNextChar

