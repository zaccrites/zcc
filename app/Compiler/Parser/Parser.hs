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
)
where


import qualified Compiler.Lexer.Token as Tokens (Token(..))
import Control.Monad.Writer (WriterT (..))
import Control.Monad.State (StateT (..), get, put, MonadState)
import Control.Monad.Identity (Identity (..))
import Control.Monad.Trans.Maybe (MaybeT (..))

import Compiler.Lexer.SourceToken
import Compiler.Parser.ParserError (ParserError)
import Compiler.Lexer.Keyword


type Identifier = String
data FuncDef = FuncDef Identifier Statement deriving (Show)
data Program = Program FuncDef deriving (Show)

data Statement = ReturnStatement Expression
  deriving (Show)

data Expression
  = ConstantExpression Integer
  | UnaryExpression UnaryOperator Expression
  deriving (Show)

data UnaryOperator
  = Negate
  | BitwiseComplement
  deriving (Show)


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
parseExpression = do
  SourceToken token _ <- getNextToken
  case token of
    Tokens.Constant value -> MaybeT . return . Just $ ConstantExpression value
    Tokens.Minus -> parseUnaryExpression Negate
    Tokens.Tilde -> parseUnaryExpression BitwiseComplement
    Tokens.OpenParen -> parseParenthesizedExpression
    _ -> MaybeT . return $ Nothing  -- TODO: emit parser error

  -- MaybeT . return $ case token of
  --   SourceToken (Tokens.Constant value) _ -> Just (ConstantExpression value)
  --   _ -> Nothing

parseUnaryExpression :: UnaryOperator -> MaybeParser Expression
parseUnaryExpression op = do
  expr <- parseExpression
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
  MaybeT . return $ if actual == expected
    then Just ()
    else Nothing   -- TODO: emit parser error



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



