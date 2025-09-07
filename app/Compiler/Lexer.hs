{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use fmap" #-}
{-# LANGUAGE DisambiguateRecordFields #-}

module Compiler.Lexer (
  getTextTokens,
  tokenToString,
  Token (..),
  SourceToken (..),
) where

import Control.Monad.State (State, get, put, runState)

import Compiler.LexerState
import Compiler.LexerError
import Compiler.SourceLocation
import Data.Char (isSpace, isDigit, isAlpha)
import Control.Monad (void, liftM)
import Control.Applicative (Const(Const))





data SourceToken = SourceToken Token SourceLocation deriving (Show)



type Lexer = State LexerState


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
  current <- get
  let withLoc token = SourceToken token current.location
  let give = return . Just . withLoc

  nextChar <- takeNextChar
  case nextChar of
    Nothing -> give EndOfFile
    Just x -> case x of
      '(' -> give OpenParen
      ')' -> give CloseParen
      '{' -> give OpenBrace
      '}' -> give CloseBrace
      ';' -> give Semicolon
      _ -> (liftM . fmap) withLoc (readOtherToken x)

  where
    readOtherToken :: Char -> Lexer (Maybe Token)
    readOtherToken x
      | isDigit x = readIntLiteralToken x
      | isIdentifierHeadChar x = Just <$> readIdentifierOrKeyword x
      | otherwise = do
        -- TODO: report error
        return Nothing


readIntLiteralToken :: Char -> Lexer (Maybe Token)
readIntLiteralToken c = do
  nextChar <- peekNextChar
  case nextChar of
    Nothing -> return . Just . Constant $ read (c:"")
    Just 'x' -> tossNextChar >> readIntLiteralOfBase 16
    Just 'X' -> tossNextChar >> readIntLiteralOfBase 16
    Just 'b' -> tossNextChar >> readIntLiteralOfBase 2
    Just 'B' -> tossNextChar >> readIntLiteralOfBase 2
    Just 'o' -> tossNextChar >> readIntLiteralOfBase 8  -- extension
    Just 'O' -> tossNextChar >> readIntLiteralOfBase 8  -- extension
    Just c2 | isDigit c2 -> if c == '0'
      then tossNextChar >> readIntLiteralOfBase 8
      else do
        tossNextChar
        restChars <- takeCharsWhile isDigit
        let value = read (c : restChars)
        return . Just . Constant $ value
    _ -> return Nothing



reconstructIntFromDigits :: Integer -> [Integer] -> Integer
reconstructIntFromDigits base digits = value
  where
    (_, value) = foldr f (1, 0) digits
    f x (mult, acc) = (mult * base, acc + mult * x)


readIntLiteralOfBase :: Integer -> Lexer (Maybe Token)
readIntLiteralOfBase base = do
  digits <- readDigits []
  case digits of
    Just xs -> do
      let value = reconstructIntFromDigits base xs
      return . Just . Constant $ value
    Nothing -> return Nothing

  where
    readDigits :: [Integer] -> Lexer (Maybe [Integer])
    readDigits digits = do
      digit <- readDigit
      case digit of
        Just value -> readDigits (value : digits)
        Nothing -> return Nothing

    readDigit :: Lexer (Maybe Integer)
    readDigit = do
      c <- peekNextChar
      case c of
        Just c' -> do
          let value = digitCharValueOfBase base c'
          case value of
            Just x -> tossNextChar >> return (Just x)
            Nothing -> do
              let msg = "invalid char in base " ++ show base ++ " int literal: '" ++ show c' ++ "'"
              emitLexerError msg
              return Nothing
        Nothing -> return Nothing




emitLexerError :: String -> Lexer ()
emitLexerError message = do
  current <- get
  let err = LexerError {
    message = message,
    location = current.location
  }
  put current { errors = current.errors ++ [err] }







readIdentifierOrKeyword :: Char -> Lexer Token
readIdentifierOrKeyword c = do
  chars <- takeCharsWhile isIdentifierTailChar
  return $ case c:chars of
    "int" -> Keyword KeywordInt
    "void" -> Keyword KeywordVoid
    "return" -> Keyword KeywordReturn
    xs -> Identifier xs


isIdentifierHeadChar :: Char -> Bool
isIdentifierHeadChar c = isAlpha c || '_' == c

isIdentifierTailChar :: Char -> Bool
isIdentifierTailChar c = isIdentifierHeadChar c || isDigit c








