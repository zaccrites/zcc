{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module Compiler.CodeGen.AssemblyX64 (
  ShowAsm (..),
  AsmProgram (..),
  AsmFuncDef (..),
  genAsmProgram,
)
where

import Control.Monad.State (StateT (..), get, put)
import Control.Monad.Identity (Identity (..))

import Compiler.CodeGen.Intermediate
import qualified Data.Map as Map


type Identifier = String
type StackOffset = Integer

data AsmProgram = AsmProgram AsmFuncDef deriving (Show)
data AsmFuncDef = AsmFuncDef Identifier Integer [AsmInstruction] deriving (Show)

data AsmInstruction
  = AsmMov AsmReadOperand AsmWriteOperand
  | AsmUnary AsmUnaryOp AsmReadOperand
  | AsmBinary AsmBinaryOp AsmReadOperand AsmWriteOperand
  | AsmIntDiv AsmReadOperand
  | AsmConvertDoubleToQuad
  | AsmAllocateStack StackOffset
  | AsmRet
  | AsmCmp AsmReadOperand AsmReadOperand
  | AsmSetIf AsmFlagCondition AsmWriteOperand
  | AsmJump Identifier
  | AsmJumpIf AsmFlagCondition Identifier
  | AsmLabel Identifier  -- it's not really an instruction, but it appears in the instruction stream
  deriving (Show)

data AsmFlagCondition
  = AsmFlagIfEqual
  | AsmFlagIfNotEqual
  | AsmFlagIfGreaterThan
  | AsmFlagIfLessThan
  | AsmFlagIfGreaterThanOrEqual
  | AsmFlagIfLessThanOrEqual
  deriving (Show)

getFlagConditionSuffix :: AsmFlagCondition -> String
getFlagConditionSuffix flag = case flag of
  AsmFlagIfEqual -> "e"
  AsmFlagIfNotEqual -> "ne"
  AsmFlagIfLessThan -> "l"
  AsmFlagIfLessThanOrEqual -> "le"
  AsmFlagIfGreaterThan -> "g"
  AsmFlagIfGreaterThanOrEqual -> "ge"


data AsmReadOperand
  = AsmReadRegister AsmReg
  | AsmReadPseudoRegister Identifier
  | AsmReadStack StackOffset
  | AsmImmVal Integer
  deriving (Show)

data AsmWriteOperand
  = AsmWriteRegister AsmReg
  | AsmWritePseudoRegister Identifier
  | AsmWriteStack StackOffset
  deriving (Show)


class ReadableAsmOperand a where
  readAsmOperand :: a -> AsmReadOperand

instance ReadableAsmOperand AsmReadOperand where
  readAsmOperand = id

instance ReadableAsmOperand AsmWriteOperand where
  readAsmOperand (AsmWriteRegister reg) = AsmReadRegister reg
  readAsmOperand (AsmWritePseudoRegister name) = AsmReadPseudoRegister name
  readAsmOperand (AsmWriteStack offset) = AsmReadStack offset


data AsmReg
  = AsmRegAX
  | AsmRegDX
  | AsmRegR10
  | AsmRegR11
  deriving (Show)

data AsmUnaryOp
  = AsmNeg
  | AsmNot
  deriving (Show)

data AsmBinaryOp
  = AsmAdd
  | AsmSub
  | AsmMul
  | AsmBitAnd
  | AsmBitOr
  | AsmBitXor
  | AsmBitShiftLeft
  | AsmBitShiftRight
  deriving (Show)


class ShowAsm a where
  showAsm :: a -> String


instance ShowAsm AsmProgram where
  showAsm (AsmProgram func) = unlines asmLines
    where
      progAsm = showAsm func
      sectionInfo = "    .section .note.GNU-stack,\"\",@progbits"
      asmLines = [progAsm, sectionInfo]


instance ShowAsm AsmFuncDef where
  showAsm (AsmFuncDef name stackSize instructions) = unlines asmLines
    where
      globalInfo = "    .globl " ++ name
      funcLabel = name ++ ":"

      preambleInstructions =
        [ "    pushq %rbp"
        , "    movq %rsp, %rbp"
        , "    subq $" ++ show stackSize ++ ", %rsp"
        ]
      implInstructions = map maybeIndentInstruction instructions
      bodyInstructions = preambleInstructions ++ implInstructions
      asmLines = [globalInfo, funcLabel] ++ bodyInstructions

      maybeIndentInstruction :: AsmInstruction -> String
      maybeIndentInstruction x =
        let text = showAsm x
        in case x of
          AsmLabel _ -> text
          _ -> "    " ++ text



instance ShowAsm AsmUnaryOp where
  showAsm AsmNot = "notl"
  showAsm AsmNeg = "negl"


instance ShowAsm AsmBinaryOp where
  showAsm AsmAdd = "addl"
  showAsm AsmSub = "subl"
  showAsm AsmMul = "imull"
  showAsm AsmBitAnd = "andl"
  showAsm AsmBitOr = "orl"
  showAsm AsmBitXor = "xorl"
  showAsm AsmBitShiftLeft = "shll"
  showAsm AsmBitShiftRight = "shrl"


instance ShowAsm AsmInstruction where
  showAsm inst = case inst of
    AsmRet -> "movq %rbp, %rsp\n    popq %rbp\n    ret"
    AsmMov src dest -> "movl " ++ showAsm src ++ ", " ++ showAsm dest
    AsmUnary op src -> showAsm op ++ " " ++ showAsm src
    AsmAllocateStack offset -> "subq $" ++ show offset ++ ", %rsp"
    AsmBinary op src dest -> showAsm op ++ " " ++ showAsm src ++ ", " ++ showAsm dest
    AsmConvertDoubleToQuad -> "cdq"
    AsmIntDiv src -> "idivl " ++ showAsm src
    AsmLabel name -> name ++ ":"

    AsmJump label -> "jmp " ++ label
    AsmJumpIf flag label -> 'j' : (getFlagConditionSuffix flag) ++ " " ++ label

    AsmCmp left right -> "cmpl " ++ showAsm left ++ ", " ++ showAsm right

    -- TODO: fix this ugly hack
    -- AsmSetIf flag dest -> "set" ++ (getFlagConditionSuffix flag) ++ " " ++ showAsm dest
    AsmSetIf flag dest ->
      let
        use8BitRegName :: AsmWriteOperand -> String
        use8BitRegName operand = case operand of
          AsmWriteRegister reg -> getReg8BitName reg
          operand' -> showAsm operand'
        destText = use8BitRegName dest
      in "set" ++ (getFlagConditionSuffix flag) ++ " " ++ destText


instance ShowAsm AsmReg where
  showAsm reg = case reg of
    AsmRegAX -> "%eax"
    AsmRegDX -> "%edx"
    AsmRegR10 -> "%r10d"
    AsmRegR11 -> "%r11d"


-- https://learn.microsoft.com/en-us/windows-hardware/drivers/debugger/x64-architecture
getReg8BitName :: AsmReg -> String
getReg8BitName reg = case reg of
  AsmRegAX -> "%al"
  AsmRegDX -> "%dl"
  AsmRegR10 -> "%r10b"
  AsmRegR11 -> "%r11b"


instance ShowAsm AsmReadOperand where
  showAsm (AsmReadRegister reg) = showAsm reg
  showAsm (AsmImmVal value) = '$' : show value
  showAsm (AsmReadPseudoRegister name) = showAsmPseudoRegister name
  showAsm (AsmReadStack offset) = showAsmStackOffset offset

instance ShowAsm AsmWriteOperand where
  showAsm (AsmWriteRegister reg) = showAsm reg
  showAsm (AsmWritePseudoRegister name) = showAsmPseudoRegister name
  showAsm (AsmWriteStack offset) = showAsmStackOffset offset


showAsmStackOffset :: StackOffset -> String
showAsmStackOffset offset = "-" ++ show offset ++ "(%rbp)"

showAsmPseudoRegister :: Identifier -> String
showAsmPseudoRegister name = "?(" ++ name ++ ")"


genAsmProgram :: IrProgram -> AsmProgram
genAsmProgram (IrProgram func) = AsmProgram $ genAsmFuncDef func

genAsmFuncDef :: IrFuncDef -> AsmFuncDef
genAsmFuncDef (IrFuncDef name instructions) =
  let
    rawAsmInstructions = concatMap genAsmInstructions instructions
    (asmInstructions, stackSize) = replacePseudoRegisters rawAsmInstructions
  in
    AsmFuncDef name stackSize asmInstructions


genAsmInstructions :: IrInstruction -> [AsmInstruction]

genAsmInstructions (IrLabel name) = [AsmLabel name]

genAsmInstructions (IrCopy src dest) =
  let
    asmSrc = genAsmReadOperand src
    asmDest = genAsmWriteOperand dest
  in [AsmMov asmSrc asmDest]


genAsmInstructions (IrReturn value) =
  [ AsmMov (genAsmReadOperand value) (AsmWriteRegister AsmRegAX)
  , AsmRet
  ]

genAsmInstructions (IrUnary IrLogicalNot src dest) =
  [ AsmCmp (AsmImmVal 0) asmSrc
  , AsmMov (AsmImmVal 0) asmDest
  , AsmSetIf AsmFlagIfEqual asmDest
  ]
  where
    asmSrc = genAsmReadOperand src
    asmDest = genAsmWriteOperand dest

genAsmInstructions (IrUnary op src dest) =
  [ AsmMov asmSrc asmDest
  , AsmUnary asmOp (readAsmOperand asmDest)
  ]
  where
    asmSrc = genAsmReadOperand src
    asmDest = genAsmWriteOperand dest
    asmOp = case op of
      IrComplement -> AsmNot
      IrNegate -> AsmNeg

-- NOTE: imul does not allow memory addresses as its destination,
-- so we have to write to a scratch register first.
genAsmInstructions (IrBinary IrMultiply left right dest) =
  [ AsmMov asmLeft scratchTarget
  , AsmBinary AsmMul asmRight scratchTarget
  , AsmMov (readAsmOperand scratchTarget) asmDest
  ]
  where
    scratchTarget = AsmWriteRegister AsmRegR11
    asmLeft = genAsmReadOperand left
    asmRight = genAsmReadOperand right
    asmDest = genAsmWriteOperand dest

genAsmInstructions (IrBinary IrDivide left right dest) =
  genAsmInstructionsIDivCommon Quotient left right dest

genAsmInstructions (IrBinary IrRemainder left right dest) =
  genAsmInstructionsIDivCommon Remainder left right dest

genAsmInstructions (IrBinary IrGreaterThan left right dest) =
  genAsmInstructionsRelationalCommon AsmFlagIfGreaterThan left right dest

genAsmInstructions (IrBinary IrLessThan left right dest) =
  genAsmInstructionsRelationalCommon AsmFlagIfLessThan left right dest

genAsmInstructions (IrBinary IrGreaterThanEqual left right dest) =
  genAsmInstructionsRelationalCommon AsmFlagIfGreaterThanOrEqual left right dest

genAsmInstructions (IrBinary IrLessThanEqual left right dest) =
  genAsmInstructionsRelationalCommon AsmFlagIfLessThanOrEqual left right dest

genAsmInstructions (IrBinary IrEqual left right dest) =
  genAsmInstructionsRelationalCommon AsmFlagIfEqual left right dest

genAsmInstructions (IrBinary IrNotEqual left right dest) =
  genAsmInstructionsRelationalCommon AsmFlagIfNotEqual left right dest

genAsmInstructions (IrBinary op left right dest) =
  [ AsmMov asmLeft asmDest
  , AsmBinary asmOp asmRight asmDest
  ]
  where
    asmLeft = genAsmReadOperand left
    asmRight = genAsmReadOperand right
    asmDest = genAsmWriteOperand dest
    asmOp = case op of
      IrAdd -> AsmAdd
      IrSubtract -> AsmSub
      IrBitwiseAnd -> AsmBitAnd
      IrBitwiseOr -> AsmBitOr
      IrBitwiseXor -> AsmBitXor
      IrBitwiseShiftLeft -> AsmBitShiftLeft
      IrBitwiseShiftRight -> AsmBitShiftRight

genAsmInstructions (IrJump condition label) = case condition of
  IrJumpAlways -> [AsmJump label]
  IrJumpIfZero src -> jumpIf AsmFlagIfEqual src
  IrJumpIfNotZero src -> jumpIf AsmFlagIfNotEqual src
  where
    jumpIf :: AsmFlagCondition -> IrReadValue -> [AsmInstruction]
    jumpIf flag src =
      [ AsmCmp (AsmImmVal 0) (genAsmReadOperand src)
      , AsmJumpIf flag label ]


genAsmInstructionsRelationalCommon :: AsmFlagCondition -> IrReadValue -> IrReadValue -> IrWriteValue -> [AsmInstruction]
genAsmInstructionsRelationalCommon flag left right dest =
  -- The second operand of a cmp instruction cannot be an immediate value.
  case asmRight of
    AsmImmVal _ ->
      [ AsmMov asmRight scratchRegister
      , AsmCmp asmLeft (readAsmOperand scratchRegister)
      , AsmMov (AsmImmVal 0) asmDest
      , AsmSetIf flag asmDest
      ]
    _ ->
      [ AsmCmp asmLeft asmRight
      , AsmMov (AsmImmVal 0) asmDest
      , AsmSetIf flag asmDest
      ]
  where
    -- What the IR thinks of as right and left get swapped
    -- in the emitted assembly code for this instruction.
    asmLeft = genAsmReadOperand right
    asmRight = genAsmReadOperand left
    asmDest = genAsmWriteOperand dest
    scratchRegister = AsmWriteRegister AsmRegR11

    evalConstant :: Integer -> Integer -> Bool
    evalConstant left' right' = case flag of
      AsmFlagIfEqual -> left' == right'
      AsmFlagIfNotEqual -> left' /= right'
      AsmFlagIfLessThan -> left' < right'
      AsmFlagIfLessThanOrEqual -> left' <= right'
      AsmFlagIfGreaterThan -> left' > right'
      AsmFlagIfGreaterThanOrEqual -> left' >= right'


data DesiredIDivOutput = Quotient | Remainder

-- NOTE: idiv only takes a single operand, which is the divisor.
-- The divisor cannot be an immediate value. The 32-bit idiv instruction
-- actually uses a 64-bit dividend, getting the 32 most-significant bits
-- from EDX and the 32 least-significant bits from EAX.
-- idiv then computes the quotient and remainder,
-- storing them in EAX and EDX respectively.
genAsmInstructionsIDivCommon :: DesiredIDivOutput -> IrReadValue -> IrReadValue -> IrWriteValue -> [AsmInstruction]
genAsmInstructionsIDivCommon output dividend divisor dest =
  -- NOTE: The scratch register is only needed if the divisor is a constant value.
  -- FUTURE: de-duplicate this
  case divisor of
    IrConstant _ ->
      [ AsmMov asmDivisor scratchTarget
      , AsmMov asmDividend (AsmWriteRegister AsmRegAX)
      , AsmConvertDoubleToQuad
      , AsmIntDiv (readAsmOperand scratchTarget)
      , AsmMov (AsmReadRegister outputSrcReg) asmDest
      ]
    _ ->
      [ AsmMov asmDividend (AsmWriteRegister AsmRegAX)
      , AsmConvertDoubleToQuad
      , AsmIntDiv asmDivisor
      , AsmMov (AsmReadRegister outputSrcReg) asmDest
      ]
  where
    scratchTarget = AsmWriteRegister AsmRegR10
    asmDividend = genAsmReadOperand dividend
    asmDivisor = genAsmReadOperand divisor
    asmDest = genAsmWriteOperand dest
    outputSrcReg = case output of
      Quotient -> AsmRegAX
      Remainder -> AsmRegDX



genAsmReadOperand :: IrReadValue -> AsmReadOperand
genAsmReadOperand (IrConstant value) = AsmImmVal value
genAsmReadOperand (IrReadVar name) = AsmReadPseudoRegister name

genAsmWriteOperand :: IrWriteValue -> AsmWriteOperand
genAsmWriteOperand (IrWriteVar name) = AsmWritePseudoRegister name


type PseudoRegReplacerState =
  ( Map.Map Identifier StackOffset  -- assigned offsets
  , StackOffset                     -- current max offset
  )

type PseudoRegReplacerT m = StateT PseudoRegReplacerState m
type PseudoRegReplacer = PseudoRegReplacerT Identity


replacePseudoRegisters :: [AsmInstruction] -> ([AsmInstruction], StackOffset)
replacePseudoRegisters instructions = (outputInstructions, stackSize - 4)
  where
    initState = (Map.empty, 4)
    (outputInstructions, (_, stackSize)) =
      runIdentity $ runStateT (replacePseudoRegisters' instructions) initState

replacePseudoRegisters' :: [AsmInstruction] -> PseudoRegReplacer [AsmInstruction]
replacePseudoRegisters' [] = return []
replacePseudoRegisters' (x:xs) = do
  newInstructions <- f x
  restInstructions <- replacePseudoRegisters' xs
  return $ newInstructions ++ restInstructions
  where
    f :: AsmInstruction -> PseudoRegReplacer [AsmInstruction]

    f (AsmMov (AsmReadPseudoRegister srcName) (AsmWritePseudoRegister destName)) = do
      newSrc <- replaceReadPseudoReg srcName
      newDest <- replaceWritePseudoReg destName
      return
        [ AsmMov newSrc (AsmWriteRegister AsmRegR10)
        , AsmMov (AsmReadRegister AsmRegR10) newDest
        ]

    f (AsmMov (AsmReadPseudoRegister srcName) dest) = do
      newSrc <- replaceReadPseudoReg srcName
      return [AsmMov newSrc dest]

    f (AsmMov src (AsmWritePseudoRegister destName)) = do
      newDest <- replaceWritePseudoReg destName
      return [AsmMov src newDest]

    -- TODO: de-duplicate, since "AsmMov" and "AsmBinary op" cases are identical
    f (AsmBinary op (AsmReadPseudoRegister srcName) (AsmWritePseudoRegister destName)) = do
      newSrc <- replaceReadPseudoReg srcName
      newDest <- replaceWritePseudoReg destName
      return
        [ AsmMov newSrc (AsmWriteRegister AsmRegR10)
        , AsmBinary op (AsmReadRegister AsmRegR10) newDest
        ]

    f (AsmBinary op (AsmReadPseudoRegister srcName) dest) = do
      newSrc <- replaceReadPseudoReg srcName
      return [AsmBinary op newSrc dest]

    f (AsmBinary op src (AsmWritePseudoRegister destName)) = do
      newDest <- replaceWritePseudoReg destName
      return [AsmBinary op src newDest]

    f (AsmCmp (AsmReadPseudoRegister leftName) (AsmReadPseudoRegister rightName)) = do
      newLeft <- replaceReadPseudoReg leftName
      newRight <- replaceReadPseudoReg rightName
      return
        [ AsmMov newLeft (AsmWriteRegister AsmRegR10)
        , AsmCmp (AsmReadRegister AsmRegR10) newRight
        ]

    f (AsmCmp (AsmReadPseudoRegister leftName) right) = do
      newLeft <- replaceReadPseudoReg leftName
      return [AsmCmp newLeft right]

    f (AsmCmp left (AsmReadPseudoRegister rightName)) = do
      newRight <- replaceReadPseudoReg rightName
      return [AsmCmp left newRight]

    f (AsmUnary op (AsmReadPseudoRegister name)) = do
      new <- replaceReadPseudoReg name
      return [AsmUnary op new]

    f (AsmIntDiv (AsmReadPseudoRegister name)) = do
      new <- replaceReadPseudoReg name
      return [AsmIntDiv new]

    f (AsmSetIf flag (AsmWritePseudoRegister name)) = do
      new <- replaceWritePseudoReg name
      return [AsmSetIf flag new]

    -- Otherwise, pass it through unchanged.
    f instruction = return [instruction]


replaceReadPseudoReg :: Identifier -> PseudoRegReplacer AsmReadOperand
replaceReadPseudoReg name = do
  offset <- assignPseudoRegStackOffset name
  return $ AsmReadStack offset

replaceWritePseudoReg :: Identifier -> PseudoRegReplacer AsmWriteOperand
replaceWritePseudoReg name = do
  offset <- assignPseudoRegStackOffset name
  return $ AsmWriteStack offset

assignPseudoRegStackOffset :: Identifier -> PseudoRegReplacer StackOffset
assignPseudoRegStackOffset name = do
  (assignments, maxOffset) <- get
  case Map.lookup name assignments of
    Just offset -> return offset
    Nothing -> do
      let assignments' = Map.insert name maxOffset assignments
      let maxOffset' = maxOffset + 4
      put (assignments', maxOffset')
      return maxOffset

