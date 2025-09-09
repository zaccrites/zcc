{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE BlockArguments #-}

module Compiler.Parser.Parser (
  parseProgram,
  Program (..),
  FuncDef (..),
  Statement (..),
  Expression (..),
  UnaryOperator (..),
  BinaryOperator (..),
)
where


import qualified Compiler.Lexer.Token as Tokens (Token(..))
import Control.Monad.Writer (WriterT (..), MonadWriter (tell))
import Control.Monad.State (StateT (..), get, put)
import Control.Monad.Identity (Identity (..))
import Control.Monad.Trans.Maybe (MaybeT (..))

import Compiler.Lexer.SourceToken
import Compiler.Parser.ParserError (ParserError (..))
import Compiler.Lexer.Keyword

import Control.Monad (void)


type Identifier = String
data FuncDef = FuncDef Identifier Statement deriving (Show)
data Program = Program FuncDef deriving (Show)

data Statement = ReturnStatement Expression
  deriving (Show)

data Expression
  = ConstantExpression Integer
  | UnaryExpression UnaryOperator Expression
  | BinaryExpression BinaryOperator Expression Expression
  deriving (Show)

data UnaryOperator
  = Negate
  | BitwiseComplement
  | LogicalNot
  deriving (Show)

data BinaryOperator
  = Add
  | Subtract
  | Multiply
  | Divide
  | Remainder
  | BitwiseShiftLeft
  | BitwiseShiftRight
  | BitwiseAnd
  | BitwiseOr
  | BitwiseXor
  | LogicalAnd
  | LogicalOr
  | Equal
  | NotEqual
  | GreaterThan
  | LessThan
  | GreaterThanEqual
  | LessThanEqual
  deriving (Show)


-- https://en.cppreference.com/w/c/language/operator_precedence.html
getBinaryOperatorPrecedence :: BinaryOperator -> Int
getBinaryOperatorPrecedence op = case op of
  Multiply -> 130
  Divide -> 130
  Remainder -> 130
  Add -> 120
  Subtract -> 120
  BitwiseShiftLeft -> 110
  BitwiseShiftRight -> 110
  LessThan -> 100
  LessThanEqual -> 100
  GreaterThan -> 100
  GreaterThanEqual -> 100
  Equal -> 90
  NotEqual -> 90
  BitwiseAnd -> 80
  BitwiseXor -> 70
  BitwiseOr -> 60
  LogicalAnd -> 50
  LogicalOr -> 40

isBinaryOperatorLeftAssociative :: BinaryOperator -> Bool
isBinaryOperatorLeftAssociative op = True



type ParserT m = WriterT [ParserError] (StateT [SourceToken] m)
type Parser = ParserT Identity
type MaybeParser = MaybeT Parser


parseProgram :: [SourceToken] -> (Maybe Program, [ParserError], [SourceToken])
parseProgram tokens = (program, errors, tokens')
  where
    ((program, errors), tokens') = runIdentity $ runStateT (runWriterT $ runMaybeT parseProgram') tokens


parseProgram' :: MaybeParser Program
parseProgram' = do
  func <- parseFuncDef
  expectToken Tokens.EndOfFile
  return $ Program func


parseFuncDef :: MaybeParser FuncDef
parseFuncDef = do
  expectKeyword KeywordInt
  name <- getIdentifier
  expectToken Tokens.OpenParen
  expectToken Tokens.CloseParen
  expectToken Tokens.OpenBrace
  stmt <- parseStatement
  expectToken Tokens.CloseBrace
  return $ FuncDef name stmt


parseStatement :: MaybeParser Statement
parseStatement = do
  expectKeyword KeywordReturn
  expr <- parseExpression
  expectToken Tokens.Semicolon
  return $ ReturnStatement expr


parseExpression :: MaybeParser Expression
parseExpression = go 0
  where
    go :: Int -> MaybeParser Expression
    go minPrec = parseFactor >>= go' minPrec

    -- Use precedence climbing to parse a binary expression.
    go' :: Int -> Expression -> MaybeParser Expression
    go' minPrec left = do
      SourceToken token _ <- peekNextToken
      case getBinaryOperator token of
        Just op -> do
          let prec = getBinaryOperatorPrecedence op
          if prec >= minPrec
            then do
              tossNextToken
              right <- go (prec + 1)
              let left' = BinaryExpression op left right
              go' minPrec left'
            else return left
        Nothing -> return left


getBinaryOperator :: Tokens.Token -> Maybe BinaryOperator
getBinaryOperator token = case token of
  Tokens.Plus -> Just Add
  Tokens.Minus -> Just Subtract
  Tokens.Asterisk -> Just Multiply
  Tokens.Slash -> Just Divide
  Tokens.Percent -> Just Remainder
  Tokens.Ampersand -> Just BitwiseAnd
  Tokens.Pipe -> Just BitwiseOr
  Tokens.Caret -> Just BitwiseXor
  Tokens.ShiftLeft -> Just BitwiseShiftLeft
  Tokens.ShiftRight -> Just BitwiseShiftRight
  Tokens.LessThan -> Just LessThan
  Tokens.LessThanEqual -> Just LessThanEqual
  Tokens.GreaterThan -> Just GreaterThan
  Tokens.GreaterThanEqual -> Just GreaterThanEqual
  Tokens.Equal -> Just Equal
  Tokens.NotEqual -> Just NotEqual
  Tokens.DoubleAmpersand -> Just LogicalAnd
  Tokens.DoublePipe -> Just LogicalOr
  _ -> Nothing


parseFactor :: MaybeParser Expression
parseFactor = do
  source@(SourceToken token _) <- getNextToken
  case token of
    Tokens.Constant value -> MaybeT . return . Just $ ConstantExpression value
    Tokens.Minus -> parseUnaryExpression Negate
    Tokens.Tilde -> parseUnaryExpression BitwiseComplement
    Tokens.Exclamation -> parseUnaryExpression LogicalNot
    Tokens.OpenParen -> parseParenthesizedExpression
    x -> do
      let message = "unexpected token \"" ++ show x ++ "\""
      tell [ParserError {message=message, token=source}]
      MaybeT . return $ Nothing


parseUnaryExpression :: UnaryOperator -> MaybeParser Expression
parseUnaryExpression op = do
  expr <- parseFactor
  MaybeT . return . Just $ UnaryExpression op expr


parseParenthesizedExpression :: MaybeParser Expression
parseParenthesizedExpression = do
  expr <- parseExpression
  expectToken Tokens.CloseParen
  MaybeT . return . Just $ expr


getIdentifier :: MaybeParser Identifier
getIdentifier = do
  token <- getNextToken
  MaybeT . return $ case token of
    SourceToken (Tokens.Identifier name) _ -> Just name
    _ -> Nothing


expectKeyword :: Keyword -> MaybeParser ()
expectKeyword keyword =
  let token = Tokens.Keyword keyword
  in expectToken token


expectToken :: Tokens.Token -> MaybeParser()
expectToken expected = do
  source@(SourceToken actual _) <- getNextToken
  if actual == expected
    then MaybeT . return $ Just ()
    else do
      let message = "expected token \"" ++ show expected ++ "\""
      tell [ParserError {token=source, message=message}]
      MaybeT . return $ Nothing



getNextToken :: MaybeParser SourceToken
getNextToken = MaybeT do
  tokens <- get
  case tokens of
    (x:xs) -> put xs >> return (Just x)
    [] -> return Nothing


peekNextToken :: MaybeParser SourceToken
peekNextToken = MaybeT do
  tokens <- get
  case tokens of
    (x:_) -> return (Just x)
    [] -> return Nothing


tossNextToken :: MaybeParser ()
tossNextToken = void getNextToken

