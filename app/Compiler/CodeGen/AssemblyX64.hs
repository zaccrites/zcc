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
  deriving (Show)

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
        [ "pushq %rbp"
        , "movq %rsp, %rbp"
        , "subq $" ++ show stackSize ++ ", %rsp"
        ]
      implInstructions = map showAsm instructions
      bodyInstructions = preambleInstructions ++ implInstructions
      bodyLines = map ("    "++) bodyInstructions

      asmLines = [globalInfo, funcLabel] ++ bodyLines


instance ShowAsm AsmReg where
  showAsm reg = case reg of
    AsmRegAX -> "%eax"
    AsmRegDX -> "%edx"
    AsmRegR10 -> "%r10d"
    AsmRegR11 -> "%r11d"


instance ShowAsm AsmUnaryOp where
  showAsm AsmNot = "notl"
  showAsm AsmNeg = "negl"


instance ShowAsm AsmBinaryOp where
  showAsm AsmAdd = "addl"
  showAsm AsmSub = "subl"
  showAsm AsmMul = "imull"


instance ShowAsm AsmInstruction where
  showAsm inst = case inst of
    AsmRet -> "movq %rbp, %rsp\n    popq %rbp\n    ret"
    AsmMov src dest -> "movl " ++ showAsm src ++ ", " ++ showAsm dest
    AsmUnary op src -> showAsm op ++ " " ++ showAsm src
    AsmAllocateStack offset -> "subq $" ++ show offset ++ ", %rsp"
    AsmBinary op src dest -> showAsm op ++ " " ++ showAsm src ++ ", " ++ showAsm dest
    AsmConvertDoubleToQuad -> "cdq"
    AsmIntDiv src -> "idivl " ++ showAsm src

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

genAsmInstructions (IrReturn value) =
  [ AsmMov (genAsmReadOperand value) (AsmWriteRegister AsmRegAX)
  , AsmRet
  ]

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

    f (AsmUnary op (AsmReadPseudoRegister name)) = do
      new <- replaceReadPseudoReg name
      return [AsmUnary op new]

    f (AsmIntDiv (AsmReadPseudoRegister name)) = do
      new <- replaceReadPseudoReg name
      return [AsmIntDiv new]

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

