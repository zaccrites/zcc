{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module Compiler.CodeGen.Intermediate (
  IrProgram (..),
  IrFuncDef (..),
  IrInstruction (..),
  IrReadValue (..),
  IrWriteValue (..),
  IrUnaryOp (..),
  genIrProgram,
)
where

import Control.Monad.Writer (WriterT (..))
import Control.Monad.State (StateT (..), get, put, MonadState)
import Control.Monad.Identity (Identity (..))
import Control.Monad.Trans.Maybe (MaybeT (..))

import Compiler.Parser.Parser


type Identifier = String

data IrProgram = IrProgram IrFuncDef deriving (Show)
data IrFuncDef = IrFuncDef Identifier [IrInstruction] deriving (Show)

data IrInstruction
  = IrReturn IrReadValue
  | IrUnary IrUnaryOp IrReadValue IrWriteValue
  deriving (Show)

data IrUnaryOp
  = IrComplement
  | IrNegate
  deriving (Show)

data IrReadValue
  = IrConstant Integer
  | IrReadVar Identifier
  deriving (Show)

data IrWriteValue
  = IrWriteVar Identifier
  deriving (Show)



-- TODO: use writer monad to output instructions?
type TempVarCounter = Integer

type IrGenT m = StateT TempVarCounter m
type IrGen = IrGenT Identity


genIrProgram :: Program -> IrProgram
genIrProgram program = result
  where
    (result, _) = runIdentity $ runStateT (genIrProgram' program) 0

genIrProgram' :: Program -> IrGen IrProgram
genIrProgram' (Program func) = IrProgram <$> genIrFuncDef func


genIrFuncDef :: FuncDef -> IrGen IrFuncDef
genIrFuncDef (FuncDef name stmt) = do
  instructions <- genStmtIrInstructions stmt
  return $ IrFuncDef name instructions


genStmtIrInstructions :: Statement -> IrGen [IrInstruction]
genStmtIrInstructions (ReturnStatement expr) = do
  (instructions, value) <- genExprIrInstructions expr
  return $ instructions ++ [IrReturn value]


genExprIrInstructions :: Expression -> IrGen ([IrInstruction], IrReadValue)
genExprIrInstructions (ConstantExpression value) = return ([], IrConstant value)
genExprIrInstructions (UnaryExpression op expr) = do
  (innerInstructions, innerValue) <- genExprIrInstructions expr
  destName <- getNewTempVarName
  let dest = IrWriteVar destName
  let instructions = [IrUnary irOp innerValue dest]
  return (innerInstructions ++ instructions, IrReadVar destName)

  where
    irOp = case op of
      Negate -> IrNegate
      BitwiseComplement -> IrComplement


getNewTempVarName :: IrGen String
getNewTempVarName = do
  count <- get
  put (count + 1)
  return $ ".tmp." ++ show count

