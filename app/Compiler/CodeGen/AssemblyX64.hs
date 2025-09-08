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
  | AsmRegR10
  deriving (Show)

data AsmUnaryOp
  = AsmNeg
  | AsmNot
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
    AsmRegR10 -> "%r10d"


instance ShowAsm AsmUnaryOp where
  showAsm AsmNot = "notl"
  showAsm AsmNeg = "negl"


instance ShowAsm AsmInstruction where
  showAsm inst = case inst of
    AsmRet -> "movq %rbp, %rsp\n    popq %rbp\n    ret"
    AsmMov src dest -> "movl " ++ showAsm src ++ ", " ++ showAsm dest
    AsmUnary op src -> showAsm op ++ " " ++ showAsm src
    AsmAllocateStack offset -> "subq $" ++ show offset ++ ", %rsp"

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

    f (AsmUnary op (AsmReadPseudoRegister name)) = do
      new <- replaceReadPseudoReg name
      return [AsmUnary op new]

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

