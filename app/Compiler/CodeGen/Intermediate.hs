{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module Compiler.CodeGen.Intermediate (
  IrProgram (..),
  IrFuncDef (..),
  IrInstruction (..),
  IrReadValue (..),
  IrWriteValue (..),
  IrUnaryOp (..),
  IrBinaryOp (..),
  IrJumpCondition (..),
  genIrProgram,
)
where

import Control.Monad.State (StateT (..), get, put)
import Control.Monad.Identity (Identity (..))

import Compiler.Parser.Parser


type Identifier = String

data IrProgram = IrProgram IrFuncDef deriving (Show)
data IrFuncDef = IrFuncDef Identifier [IrInstruction] deriving (Show)

data IrInstruction
  = IrReturn IrReadValue
  | IrUnary IrUnaryOp IrReadValue IrWriteValue
  | IrBinary IrBinaryOp IrReadValue IrReadValue IrWriteValue
  | IrCopy IrReadValue IrWriteValue
  | IrJump IrJumpCondition Identifier
  | IrLabel Identifier
  deriving (Show)

data IrJumpCondition
  = IrJumpAlways
  | IrJumpIfZero IrReadValue
  | IrJumpIfNotZero IrReadValue
  deriving (Show)

data IrUnaryOp
  = IrComplement
  | IrNegate
  | IrLogicalNot
  deriving (Show)

data IrBinaryOp
  = IrAdd
  | IrSubtract
  | IrMultiply
  | IrDivide
  | IrRemainder
  | IrBitwiseAnd
  | IrBitwiseOr
  | IrBitwiseXor
  | IrBitwiseShiftLeft
  | IrBitwiseShiftRight
  | IrLessThan
  | IrLessThanEqual
  | IrGreaterThan
  | IrGreaterThanEqual
  | IrEqual
  | IrNotEqual
  deriving (Show)

data IrReadValue
  = IrConstant Integer
  | IrReadVar Identifier
  deriving (Show)

data IrWriteValue
  = IrWriteVar Identifier
  deriving (Show)


type TempVarCounter = Integer
type LabelCounter = Integer
type IrGenState = (TempVarCounter, LabelCounter)

type IrGenT m = StateT IrGenState m
type IrGen = IrGenT Identity


genIrProgram :: Program -> IrProgram
genIrProgram program = result
  where
    (result, _) = runIdentity $ runStateT (genIrProgram' program) (0, 0)

genIrProgram' :: Program -> IrGen IrProgram
genIrProgram' (Program func) = IrProgram <$> genIrFuncDef func


genIrFuncDef :: FuncDef -> IrGen IrFuncDef
genIrFuncDef (FuncDef name items) = do
  -- main() returns 0 at the end if there is no return statement.
  -- The behavior for other functions is undefined if they don't
  -- explicitly return a value, so returning 0 is allowed.
  -- We'll let a future stage optimize this away if it isn't needed.
  instructions <- concat <$> mapM genBlockItemIrInstructions items
  let instructions' = instructions ++ [IrReturn (IrConstant 0)]
  return $ IrFuncDef name instructions'


genBlockItemIrInstructions :: BlockItem -> IrGen [IrInstruction]
genBlockItemIrInstructions item = case item of
  BlockItemStatement stmt -> genStmtIrInstructions stmt
  BlockItemDeclaration decl -> genDeclIrInstructions decl


genDeclIrInstructions :: Declaration -> IrGen [IrInstruction]
genDeclIrInstructions (VariableDeclaration name initExpr) =
  case initExpr of
    Just expr -> do
      (instructions, value) <- genExprIrInstructions expr
      return $ instructions ++ [IrCopy value (IrWriteVar name)]
    Nothing -> return []


genStmtIrInstructions :: Statement -> IrGen [IrInstruction]
genStmtIrInstructions (ReturnStatement expr) = do
  (instructions, value) <- genExprIrInstructions expr
  return $ instructions ++ [IrReturn value]

genStmtIrInstructions (ExpressionStatement expr) = do
  (instructions, _) <- genExprIrInstructions expr
  return instructions

genStmtIrInstructions NullStatement = return []


genExprIrInstructions :: Expression -> IrGen ([IrInstruction], IrReadValue)
genExprIrInstructions (ConstantExpression value) = return ([], IrConstant value)

genExprIrInstructions (AssignmentExpression (VariableExpression name) expr) = do
  (exprInstructions, exprValue) <- genExprIrInstructions expr
  let instructions = exprInstructions ++ [IrCopy exprValue (IrWriteVar name)]
  return (instructions, exprValue)

-- TODO: redo the AST types to make this impossible?
genExprIrInstructions (AssignmentExpression _ _) = undefined

genExprIrInstructions (VariableExpression name) = return ([], IrReadVar name)

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
      LogicalNot -> IrLogicalNot

genExprIrInstructions (BinaryExpression LogicalAnd left right) = do
  (innerInstructionsLeft, innerValueLeft) <- genExprIrInstructions left
  (innerInstructionsRight, innerValueRight) <- genExprIrInstructions right
  destName <- getNewTempVarName
  falseLabel <- getNewLabelName "andFalse"
  endLabel <- getNewLabelName "andEnd"
  let dest = IrWriteVar destName
  let instructions =
        innerInstructionsLeft ++
        [IrJump (IrJumpIfZero innerValueLeft) falseLabel] ++
        innerInstructionsRight ++
        [ IrJump (IrJumpIfZero innerValueRight) falseLabel
        , IrCopy (IrConstant 1) dest
        , IrJump IrJumpAlways endLabel
        , IrLabel falseLabel
        , IrCopy (IrConstant 0) dest
        , IrLabel endLabel
        ]
  return (instructions, IrReadVar destName)


genExprIrInstructions (BinaryExpression LogicalOr left right) = do
  (innerInstructionsLeft, innerValueLeft) <- genExprIrInstructions left
  (innerInstructionsRight, innerValueRight) <- genExprIrInstructions right
  destName <- getNewTempVarName
  trueLabel <- getNewLabelName "orTrue"
  endLabel <- getNewLabelName "orEnd"
  let dest = IrWriteVar destName
  let instructions =
        innerInstructionsLeft ++
        [IrJump (IrJumpIfNotZero innerValueLeft) trueLabel] ++
        innerInstructionsRight ++
        [ IrJump (IrJumpIfNotZero innerValueRight) trueLabel
        , IrCopy (IrConstant 0) dest
        , IrJump IrJumpAlways endLabel
        , IrLabel trueLabel
        , IrCopy (IrConstant 1) dest
        , IrLabel endLabel
        ]
  return (instructions, IrReadVar destName)


genExprIrInstructions (BinaryExpression op left right) = do
  (innerInstructionsLeft, innerValueLeft) <- genExprIrInstructions left
  (innerInstructionsRight, innerValueRight) <- genExprIrInstructions right
  destName <- getNewTempVarName
  let dest = IrWriteVar destName
  let instructions = [IrBinary irOp innerValueLeft innerValueRight dest]
  let allInstructions = concat [innerInstructionsLeft, innerInstructionsRight, instructions]
  return (allInstructions, IrReadVar destName)

  where
    irOp = case op of
      Add -> IrAdd
      Subtract -> IrSubtract
      Multiply -> IrMultiply
      Divide -> IrDivide
      Remainder -> IrRemainder
      BitwiseAnd -> IrBitwiseAnd
      BitwiseOr -> IrBitwiseOr
      BitwiseXor -> IrBitwiseXor
      BitwiseShiftLeft -> IrBitwiseShiftLeft
      BitwiseShiftRight -> IrBitwiseShiftRight
      LessThan -> IrLessThan
      LessThanEqual -> IrLessThanEqual
      GreaterThan -> IrGreaterThan
      GreaterThanEqual -> IrGreaterThanEqual
      Equal -> IrEqual
      NotEqual -> IrNotEqual
      Assign -> undefined  -- TODO: FIX THIS! this shouldn't ever happen because of AssignmentExpression, so this is a bad abstraction


getNewTempVarName :: IrGen String
getNewTempVarName = do
  (tempVarCount, labelCount) <- get
  put (tempVarCount + 1, labelCount)
  return $ ".tmp." ++ show (tempVarCount + 1)


-- FUTURE: move "implementation details" like the ".L" prefix for local
-- labels on Linux to the AssemblyX64 stage.
getNewLabelName :: String -> IrGen Identifier
getNewLabelName prefix = do
  (tempVarCount, labelCount) <- get
  put (tempVarCount, labelCount + 1)
  return $ ".L_" ++ prefix ++ "." ++ show (labelCount + 1)

