
module Compiler.Lexer.IntLexing (
  readIntLiteralToken,
)
where


import Compiler.Lexer.Lexer
import Compiler.Lexer.Token


readIntLiteralToken :: Lexer (Maybe Token)
readIntLiteralToken = do
  nextChar <- peekNextChar
  case nextChar of
    Just '0' -> tossNextChar >> readPrefixedIntLiteralToken
    Just _ -> readIntLiteralOfBase 10
    _ -> return Nothing


readPrefixedIntLiteralToken :: Lexer (Maybe Token)
readPrefixedIntLiteralToken = do
  nextChar <- peekNextChar
  case nextChar of
    Just 'x' -> tossNextChar >> readIntLiteralOfBase 16
    Just 'X' -> tossNextChar >> readIntLiteralOfBase 16
    Just 'b' -> tossNextChar >> readIntLiteralOfBase 2
    Just 'B' -> tossNextChar >> readIntLiteralOfBase 2
    Just _ -> readIntLiteralOfBase 8
    Nothing -> return Nothing


readIntLiteralOfBase :: Integer -> Lexer (Maybe Token)
readIntLiteralOfBase base = do
  value <- readDigits 0
  return . Just . Constant $ value

  where
    readDigits :: Integer -> Lexer Integer
    readDigits acc = do
      digit <- readDigit
      case digit of
        Just value -> readDigits (acc * base + value)
        Nothing -> return acc

    readDigit :: Lexer (Maybe Integer)
    readDigit = do
      c <- peekNextChar
      case c of
        Just c' -> do
          case digitCharValueOfBase base c' of
            DigitValue value -> do
              tossNextChar
              return $ Just value
            DigitOutOfRange -> do
              let msg = "invalid char in " ++ getIntBaseName base ++ " integer literal: '" ++ (c' : "'")
              emitLexerError msg
              return Nothing
            InvalidDigit -> return Nothing
        Nothing -> return Nothing


getIntBaseName :: Integer -> String
getIntBaseName base = case base of
  2 -> "binary"
  8 -> "octal"
  10 -> "decimal"
  16 -> "hex"
  n -> "base " ++ show n


data DigitValueResult
  = DigitValue Integer
  | DigitOutOfRange
  | InvalidDigit


digitCharValueOfBase :: Integer -> Char -> DigitValueResult
digitCharValueOfBase base c =
  case digitCharValue c of
    Just value -> if value < base
      then DigitValue value
      else DigitOutOfRange
    Nothing -> InvalidDigit


digitCharValue :: Char -> Maybe Integer
digitCharValue c = case c of
  '0' -> Just 0
  '1' -> Just 1
  '2' -> Just 2
  '3' -> Just 3
  '4' -> Just 4
  '5' -> Just 5
  '6' -> Just 6
  '7' -> Just 7
  '8' -> Just 8
  '9' -> Just 9
  'a' -> Just 0xa
  'b' -> Just 0xb
  'c' -> Just 0xc
  'd' -> Just 0xd
  'e' -> Just 0xe
  'f' -> Just 0xf
  'A' -> Just 0xA
  'B' -> Just 0xB
  'C' -> Just 0xC
  'D' -> Just 0xD
  'E' -> Just 0xE
  'F' -> Just 0xF
  _ -> Nothing

