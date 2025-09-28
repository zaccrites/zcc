{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module Compiler.CodeGen.Intermediate (
  IrProgram (..),
  IrDefinition (..),
  IrInstruction (..),
  IrReadValue (..),
  IrWriteValue (..),
  IrUnaryOp (..),
  IrBinaryOp (..),
  IrJumpCondition (..),
  ReadIrValue (..),
  genIrProgram,
)
where

import Control.Monad.State (StateT (..), get, put)
import Control.Monad.Identity (Identity (..))

import Compiler.Parser.Parser
import Data.Maybe (fromMaybe, catMaybes)
import Data.Foldable (foldrM)


type Identifier = String

data IrDefinition
  = IrFunctionDefinition Identifier [FunctionParameter] [IrInstruction]
  deriving (Show)

data IrProgram = IrProgram [IrDefinition] deriving (Show)


data IrInstruction
  = IrReturn IrReadValue
  | IrUnary IrUnaryOp IrReadValue IrWriteValue
  | IrBinary IrBinaryOp IrReadValue IrReadValue IrWriteValue
  | IrCopy IrReadValue IrWriteValue
  | IrJump IrJumpCondition Identifier
  | IrLabel Identifier
  | IrFunctionCall Identifier [IrReadValue] IrWriteValue
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
  deriving (Show, Eq)

data IrWriteValue
  = IrWriteVar Identifier
  deriving (Show, Eq)


class ReadIrValue a where
  readIrValue :: a -> IrReadValue

instance ReadIrValue IrReadValue where
  readIrValue = id

instance ReadIrValue IrWriteValue where
  readIrValue (IrWriteVar name) = IrReadVar name


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
genIrProgram' (Program decls) = IrProgram . catMaybes <$> mapM genIrDecl decls


-- genIrFunctionDefinition :: FunctionDefinition -> IrGen IrFunctionDefinition
-- genIrFunctionDefinition (FunctionDefinition name args block) = do
--   -- main() returns 0 at the end if there is no return statement.
--   -- The behavior for other functions is undefined if they don't
--   -- explicitly return a value, so returning 0 is allowed.
--   -- We'll let a future stage optimize this away if it isn't needed.
--   instructions <- genBlockIrInstructions block
--   let instructions' = instructions ++ [IrReturn (IrConstant 0)]
--   return $ IrFunctionDefinition name instructions'


genIrDecl :: Declaration -> IrGen (Maybe IrDefinition)
genIrDecl decl@(VariableDeclaration {}) = undefined  -- TODO
genIrDecl decl@(FunctionDeclaration linkage name params Nothing) = return Nothing
genIrDecl decl@(FunctionDeclaration linkage name params (Just _)) = do
  -- declInstructions <- genDeclIrInstructions decl
  Just . IrFunctionDefinition name params <$> genDeclIrInstructions decl


genBlockIrInstructions :: Block -> IrGen [IrInstruction]
genBlockIrInstructions block = concat <$> mapM genBlockItemIrInstructions block


genBlockItemIrInstructions :: BlockItem -> IrGen [IrInstruction]
genBlockItemIrInstructions item = case item of
  BlockItemStatement stmt -> genStmtIrInstructions stmt
  BlockItemDeclaration decl -> genDeclIrInstructions decl
  BlockItemLabel name -> genLabelIrInstructions name


genLabelIrInstructions :: Identifier -> IrGen [IrInstruction]
genLabelIrInstructions labelName = do
  let name = makeFuncLocalLabelName labelName
  return [IrLabel name]


genDeclIrInstructions :: Declaration -> IrGen [IrInstruction]
genDeclIrInstructions (FunctionDeclaration _ _ _ Nothing) = return []

genDeclIrInstructions (FunctionDeclaration _ name _ (Just block)) =
  addInstructions <$> genBlockIrInstructions block
  where
    -- main() returns 0 at the end if there is no return statement.
    -- The behavior for other functions is undefined if they don't
    -- explicitly return a value, so returning 0 is allowed.
    -- We'll let a future stage optimize this away if it isn't needed.
    addInstructions :: [IrInstruction] -> [IrInstruction]
    addInstructions xs = case name of
      "main" -> xs ++ [IrReturn (IrConstant 0)]
      _ -> xs

genDeclIrInstructions (VariableDeclaration linkage name initExpr) =
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

genStmtIrInstructions (CompoundStatement block) =
  genBlockIrInstructions block

genStmtIrInstructions (IfStatement expr stmt Nothing) = do
  (exprInstructions, exprValue) <- genExprIrInstructions expr
  stmtInstructions <- genStmtIrInstructions stmt
  endLabelName <- getNewLabelName "ifEnd"
  return
    (
      exprInstructions ++
      [IrJump (IrJumpIfZero exprValue) endLabelName] ++
      stmtInstructions ++
      [IrLabel endLabelName]
    )


genStmtIrInstructions (IfStatement expr stmt (Just elseStmt)) = do
  (exprInstructions, exprValue) <- genExprIrInstructions expr
  stmtInstructions <- genStmtIrInstructions stmt
  elseStmtInstructions <- genStmtIrInstructions elseStmt
  elseLabelName <- getNewLabelName "ifElse"
  endLabelName <- getNewLabelName "ifEnd"
  return
    (
      exprInstructions ++
      [IrJump (IrJumpIfZero exprValue) elseLabelName] ++
      stmtInstructions ++
      [ IrJump IrJumpAlways endLabelName
      , IrLabel elseLabelName
      ] ++
      elseStmtInstructions ++
      [IrLabel endLabelName]
    )


genStmtIrInstructions NullStatement = return []

genStmtIrInstructions (GotoStatement name) =
  return [IrJump IrJumpAlways (makeFuncLocalLabelName name)]


genStmtIrInstructions (WhileStatement expr stmt label) = do
  (exprInstructions, exprValue) <- genExprIrInstructions expr
  stmtInstructions <- genStmtIrInstructions stmt
  let continueLabelName = getLoopContinueTargetLabel label
  let breakLabelName = getBreakTargetLabel (BreakTargetLoop label)
  return (
    [ IrLabel continueLabelName ] ++
    exprInstructions ++
    [ IrJump (IrJumpIfZero exprValue) breakLabelName ] ++
    stmtInstructions ++
    [ IrJump IrJumpAlways continueLabelName
    , IrLabel breakLabelName
    ]
    )

genStmtIrInstructions (DoWhileStatement expr stmt label) = do
  (exprInstructions, exprValue) <- genExprIrInstructions expr
  stmtInstructions <- genStmtIrInstructions stmt
  let startLabelName = getLoopStartTargetLabel label
  let continueLabelName = getLoopContinueTargetLabel label
  let breakLabelName = getBreakTargetLabel (BreakTargetLoop label)
  return (
    [ IrLabel startLabelName ] ++
    stmtInstructions ++
    [ IrLabel continueLabelName ] ++
    exprInstructions ++
    [ IrJump (IrJumpIfNotZero exprValue) startLabelName
    , IrLabel breakLabelName
    ]
    )

genStmtIrInstructions (ForStatement forInit (Just expr) forPost stmt label) = do
  forInitInstructions <- genForInitInstructions forInit
  (exprInstructions, exprValue) <- genExprIrInstructions expr
  forPostInstructions <- traverse genExprStmtIrInstructions forPost
  stmtInstructions <- genStmtIrInstructions stmt
  let continueLabelName = getLoopContinueTargetLabel label
  let breakLabelName = getBreakTargetLabel (BreakTargetLoop label)

  return (
    forInitInstructions ++
    [ IrLabel continueLabelName ] ++
    exprInstructions ++
    [ IrJump (IrJumpIfZero exprValue) breakLabelName ] ++
    stmtInstructions ++
    fromMaybe [] forPostInstructions ++
    [ IrJump IrJumpAlways continueLabelName
    , IrLabel breakLabelName
    ]
    )


genStmtIrInstructions (ForStatement forInit Nothing forPost stmt label) = do
  forInitInstructions <- genForInitInstructions forInit
  forPostInstructions <- traverse genExprStmtIrInstructions forPost
  stmtInstructions <- genStmtIrInstructions stmt
  let continueLabelName = getLoopContinueTargetLabel label
  let breakLabelName = getBreakTargetLabel (BreakTargetLoop label)

  return (
    forInitInstructions ++
    [ IrLabel continueLabelName ] ++
    stmtInstructions ++
    fromMaybe [] forPostInstructions ++
    [ IrJump IrJumpAlways continueLabelName
    , IrLabel breakLabelName
    ]
    )


genStmtIrInstructions (ContinueStatement label) =
  return [IrJump IrJumpAlways (getLoopContinueTargetLabel label)]

genStmtIrInstructions (BreakStatement label) =
  return [IrJump IrJumpAlways (getBreakTargetLabel label)]


genStmtIrInstructions (SwitchStatement expr items label) = do
  (exprInstructions, exprValue) <- genExprIrInstructions expr
  scratchpad <- IrWriteVar <$> getNewTempVarName
  let switchBreakLabel = getBreakTargetLabel (BreakTargetSwitch label)
  stmtInstructions <- switchStatementInstructions
  return (
    exprInstructions ++
    caseJumpInstructions scratchpad exprValue ++
    [IrJump IrJumpAlways switchBreakLabel] ++  -- needed if there is no 'default' case
    stmtInstructions ++
    [IrLabel switchBreakLabel]
    )

  where
    caseJumpInstructions :: IrWriteValue -> IrReadValue -> [IrInstruction]
    caseJumpInstructions scratchpad exprValue = concat $ foldr f [] items
      where
        f :: SwitchItem -> [[IrInstruction]] -> [[IrInstruction]]
        f (SwitchItemCase value caseLabel) acc =
          [ IrBinary IrEqual (IrConstant value) exprValue scratchpad
          , IrJump (IrJumpIfNotZero (readIrValue scratchpad)) (makeFuncLocalLabelName caseLabel)
          ] : acc
        f (SwitchItemDefaultCase caseLabel) acc =
          [ IrJump IrJumpAlways (makeFuncLocalLabelName caseLabel) ] : acc
        f (SwitchItemStatement _) acc = acc

    switchStatementInstructions :: IrGen [IrInstruction]
    switchStatementInstructions = concat <$> foldrM f [] items
      where
        f :: SwitchItem -> [[IrInstruction]] -> IrGen [[IrInstruction]]
        f (SwitchItemCase _ caseLabel) acc =
          return $ [IrLabel (makeFuncLocalLabelName caseLabel)] : acc
        f (SwitchItemDefaultCase caseLabel) acc
          = return $ [IrLabel (makeFuncLocalLabelName caseLabel)] : acc
        f (SwitchItemStatement stmt) acc
          = (: acc) <$> genStmtIrInstructions stmt


genForInitInstructions :: ForInit -> IrGen [IrInstruction]
genForInitInstructions ForInitEmpty = return []
genForInitInstructions (ForInitExpression expr) = genExprStmtIrInstructions expr
genForInitInstructions (ForInitDeclaration decl) = case decl of
  VariableDeclaration linkage name (Just initExpr) -> do
    (instructions, value) <- genExprIrInstructions initExpr
    return $ instructions ++ [IrCopy value (IrWriteVar name)]
  _ -> return []


-- Generate the instructions for an expression, ignoring the value.
genExprStmtIrInstructions :: Expression -> IrGen [IrInstruction]
genExprStmtIrInstructions expr = fst <$> genExprIrInstructions expr


getBreakTargetLabel :: BreakTarget -> Identifier
getBreakTargetLabel target =
  ".L_" ++ targetTypeName ++ "." ++ name ++ ".break"
  where
    (targetTypeName, name) = case target of
      (BreakTargetLoop (LoopLabel name')) -> ("loop", name')
      (BreakTargetSwitch (SwitchLabel name')) -> ("switch", name')

getLoopContinueTargetLabel :: ContinueTarget -> Identifier
getLoopContinueTargetLabel (LoopLabel name) = ".L_loop." ++ name ++ ".continue"

-- Only really needed for do-while loops.
getLoopStartTargetLabel :: LoopLabel -> Identifier
getLoopStartTargetLabel (LoopLabel name) = ".L_loop." ++ name ++ ".start"


genExprIrInstructions :: Expression -> IrGen ([IrInstruction], IrReadValue)
genExprIrInstructions (ConstantExpression value) = return ([], IrConstant value)

genExprIrInstructions (BinaryExpression Assign left right) =
  genAssignmentExprIrInstructions AssignmentPlain left right

genExprIrInstructions (BinaryExpression PlusAssign left right) =
  genAssignmentExprIrInstructions AssignmentPlus left right

genExprIrInstructions (BinaryExpression MinusAssign left right) =
  genAssignmentExprIrInstructions AssignmentMinus left right

genExprIrInstructions (BinaryExpression MultiplyAssign left right) =
  genAssignmentExprIrInstructions AssignmentMultiply left right

genExprIrInstructions (BinaryExpression DivideAssign left right) =
  genAssignmentExprIrInstructions AssignmentDivide left right

genExprIrInstructions (BinaryExpression RemainderAssign left right) =
  genAssignmentExprIrInstructions AssignmentRemainder left right

genExprIrInstructions (BinaryExpression BitwiseAndAssign left right) =
  genAssignmentExprIrInstructions AssignmentBitwiseAnd left right

genExprIrInstructions (BinaryExpression BitwiseOrAssign left right) =
  genAssignmentExprIrInstructions AssignmentBitwiseOr left right

genExprIrInstructions (BinaryExpression BitwiseXorAssign left right) =
  genAssignmentExprIrInstructions AssignmentBitwiseXor left right

genExprIrInstructions (BinaryExpression BitwiseShiftLeftAssign left right) =
  genAssignmentExprIrInstructions AssignmentBitwiseShiftLeft left right

genExprIrInstructions (BinaryExpression BitwiseShiftRightAssign left right) =
  genAssignmentExprIrInstructions AssignmentBitwiseShiftRight left right


genExprIrInstructions (VariableExpression name) = return ([], IrReadVar name)


genExprIrInstructions (UnaryExpression PrefixIncrement (VariableExpression name)) = do
  let instructions = [IrBinary IrAdd (IrReadVar name) (IrConstant 1) (IrWriteVar name)]
  return (instructions, IrReadVar name)

genExprIrInstructions (UnaryExpression PrefixDecrement (VariableExpression name)) = do
  let instructions = [IrBinary IrSubtract (IrReadVar name) (IrConstant 1) (IrWriteVar name)]
  return (instructions, IrReadVar name)

genExprIrInstructions (UnaryExpression PostfixIncrement (VariableExpression name)) = do
  tempName <- getNewTempVarName
  let instructions =
        [ IrCopy (IrReadVar name) (IrWriteVar tempName)
        , IrBinary IrAdd (IrReadVar name) (IrConstant 1) (IrWriteVar name)
        ]
  return (instructions, IrReadVar tempName)

genExprIrInstructions (UnaryExpression PostfixDecrement (VariableExpression name)) = do
  tempName <- getNewTempVarName
  let instructions =
        [ IrCopy (IrReadVar name) (IrWriteVar tempName)
        , IrBinary IrSubtract (IrReadVar name) (IrConstant 1) (IrWriteVar name)
        ]
  return (instructions, IrReadVar tempName)

-- FUTURE: will need to support more sophisticated lvalues: e.g. "*x++ += 10;"
genExprIrInstructions (UnaryExpression PrefixIncrement _) = undefined
genExprIrInstructions (UnaryExpression PrefixDecrement _) = undefined
genExprIrInstructions (UnaryExpression PostfixIncrement _) = undefined
genExprIrInstructions (UnaryExpression PostfixDecrement _) = undefined


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


genExprIrInstructions (ConditionalExpression cond ifTrue ifFalse) = do
  (condInstructions, condValue) <- genExprIrInstructions cond
  (ifTrueInstructions, ifTrueValue) <- genExprIrInstructions ifTrue
  (ifFalseInstructions, ifFalseValue) <- genExprIrInstructions ifFalse
  falseLabelName <- getNewLabelName "condtionalFalse"
  endLabelName <- getNewLabelName "condtionalEnd"
  outputName <- getNewTempVarName
  let instructions =
        condInstructions ++
        [ IrJump (IrJumpIfZero condValue) falseLabelName ] ++
        ifTrueInstructions ++
        [ IrCopy ifTrueValue (IrWriteVar outputName)
        , IrJump IrJumpAlways endLabelName
        , IrLabel falseLabelName
        ] ++
        ifFalseInstructions ++
        [ IrCopy ifFalseValue (IrWriteVar outputName)
        , IrLabel endLabelName
        ]
  return (instructions, IrReadVar outputName)


genExprIrInstructions (FunctionCallExpression name args) = do
  args' <- traverse genExprIrInstructions args
  let argsInstructions = concatMap fst args'
  let argsReadValues = map snd args'
  resultValue <- IrWriteVar <$> getNewTempVarName
  let instructions = argsInstructions ++ [IrFunctionCall name argsReadValues resultValue]
  return (instructions, readIrValue resultValue)



data AssignmentType
  = AssignmentPlain
  | AssignmentPlus
  | AssignmentMinus
  | AssignmentMultiply
  | AssignmentDivide
  | AssignmentRemainder
  | AssignmentBitwiseAnd
  | AssignmentBitwiseOr
  | AssignmentBitwiseXor
  | AssignmentBitwiseShiftLeft
  | AssignmentBitwiseShiftRight


genAssignmentExprIrInstructions :: AssignmentType -> Expression -> Expression -> IrGen ([IrInstruction], IrReadValue)
genAssignmentExprIrInstructions assignmentType (VariableExpression name) expr = do
  (exprInstructions, exprValue) <- genExprIrInstructions expr
  let makeIrInstruction op = IrBinary op (IrReadVar name) exprValue (IrWriteVar name)
  let writebackInstruction = case assignmentType of
        AssignmentPlain -> IrCopy exprValue (IrWriteVar name)
        AssignmentPlus -> makeIrInstruction IrAdd
        AssignmentMinus -> makeIrInstruction IrSubtract
        AssignmentMultiply -> makeIrInstruction IrMultiply
        AssignmentDivide -> makeIrInstruction IrDivide
        AssignmentRemainder -> makeIrInstruction IrRemainder
        AssignmentBitwiseAnd -> makeIrInstruction IrBitwiseAnd
        AssignmentBitwiseOr -> makeIrInstruction IrBitwiseOr
        AssignmentBitwiseXor -> makeIrInstruction IrBitwiseXor
        AssignmentBitwiseShiftLeft -> makeIrInstruction IrBitwiseShiftLeft
        AssignmentBitwiseShiftRight -> makeIrInstruction IrBitwiseShiftRight
  let instructions = exprInstructions ++ [writebackInstruction]
  return (instructions, exprValue)

-- FUTURE: will need to support more sophisticated lvalues: e.g. "*x++ += 10;"
genAssignmentExprIrInstructions _ _ _ = undefined


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


makeFuncLocalLabelName :: Identifier -> Identifier
makeFuncLocalLabelName labelName = ".L_" ++ labelName

