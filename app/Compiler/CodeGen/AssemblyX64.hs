{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module Compiler.CodeGen.AssemblyX64 (
  ShowAsm (..),
  AsmProgram,
  genProgramAsm,
)
where

import Compiler.Parser.Parser


type Identifier = String

data AsmProgram = AsmProgram AsmFuncDef deriving (Show)
data AsmFuncDef = AsmFuncDef Identifier [AsmInstruction] deriving (Show)

data AsmInstruction
  = AsmMov AsmOperand AsmOperand
  | AsmRet
  deriving (Show)

data AsmOperand
  = AsmRegister AsmReg
  | AsmImmVal Integer
  deriving (Show)

data AsmReg
  = AsmRegAX
  | AsmRegR10
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
  showAsm (AsmFuncDef name instructions) = unlines asmLines
    where
      globalInfo = "    .globl " ++ name
      funcLabel = name ++ ":"
      bodyLines = map(("    "++) . showAsm) instructions
      asmLines = [globalInfo, funcLabel] ++ bodyLines


instance ShowAsm AsmReg where
  showAsm reg = case reg of
    AsmRegAX -> "%eax"
    AsmRegR10 -> "TODO"


instance ShowAsm AsmInstruction where
  showAsm inst = case inst of
    -- AsmRet -> "mov rsp, rbp\n    pop rbp\n    ret"
    AsmRet -> "ret"
    AsmMov src dest -> "movl " ++ showAsm src ++ ", " ++ showAsm dest


instance ShowAsm AsmOperand where
  showAsm (AsmRegister reg) = showAsm reg
  showAsm (AsmImmVal value) = '$' : show value




genProgramAsm :: Program -> AsmProgram
genProgramAsm (Program func) = AsmProgram $ genFuncDefAsm func

genFuncDefAsm :: FuncDef -> AsmFuncDef
genFuncDefAsm (FuncDef name stmt) = AsmFuncDef name (genStmtAsm stmt)

genStmtAsm :: Statement -> [AsmInstruction]
genStmtAsm (ReturnStatement (ConstantExpression value)) =
  [ AsmMov (AsmImmVal value) (AsmRegister AsmRegAX)
  , AsmRet
  ]



