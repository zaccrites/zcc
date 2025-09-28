{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module Compiler.CodeGen.AssemblyX64.Base (
  genAsmProgram,
  ShowAsm (..),
  AsmProgram (..),
  AsmDefinition (..),
  Identifier,
  StackOffset,
  AsmInstruction (..),
  AsmWriteOperand (..),
  AsmReadOperand (..),
  AsmReg (..),
)
where

import Data.List (intercalate, genericLength)

import Compiler.CodeGen.Intermediate
import Compiler.CodeGen.AssemblyX64.Types
import Compiler.Parser.Parser (FunctionParameter (..))


data AsmProgram = AsmProgram [AsmDefinition] deriving (Show)
data AsmDefinition
  = AsmFuncDef Identifier Integer [AsmInstruction]
  deriving (Show)

data AsmInstruction
  = AsmMov AsmReadOperand AsmWriteOperand
  | AsmUnary AsmUnaryOp AsmReadOperand
  | AsmBinary AsmBinaryOp AsmReadOperand AsmWriteOperand
  | AsmIntDiv AsmReadOperand
  | AsmConvertDoubleToQuad
  | AsmAllocateStack StackOffset
  | AsmDeallocateStack StackOffset
  | AsmPush AsmReadOperand
  | AsmPop AsmWriteOperand
  | AsmCall Identifier
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
  | AsmRegBX
  | AsmRegCX
  | AsmRegDX
  | AsmRegDI
  | AsmRegSI
  | AsmRegBP
  | AsmRegSP
  | AsmRegR8
  | AsmRegR9
  | AsmRegR10
  | AsmRegR11
  | AsmRegR12
  | AsmRegR13
  | AsmRegR14
  | AsmRegR15
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
  showAsm (AsmProgram defs) = unlines asmLines
    where
      progAsm = intercalate "\n\n" . map showAsm $ defs
      sectionInfo = "    .section .note.GNU-stack,\"\",@progbits"
      asmLines = [progAsm, sectionInfo]


instance ShowAsm AsmDefinition where
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
    AsmDeallocateStack offset -> "addq $" ++ show offset ++ ", %rsp"
    AsmBinary op src dest -> showAsm op ++ " " ++ showAsm src ++ ", " ++ showAsm dest
    AsmConvertDoubleToQuad -> "cdq"
    AsmIntDiv src -> "idivl " ++ showAsm src
    AsmLabel name -> name ++ ":"

    AsmJump label -> "jmp " ++ label
    AsmJumpIf flag label -> 'j' : getFlagConditionSuffix flag ++ " " ++ label

    AsmCmp left right -> "cmpl " ++ showAsm left ++ ", " ++ showAsm right

    AsmCall name -> "call " ++ name
    AsmPush x -> "pushq " ++ showAsm x
    AsmPop x -> "popq " ++ showAsm x

    AsmSetIf flag dest ->
      let
        destText = case dest of
          AsmWriteRegister reg -> getAsmRegName OpSizeByte reg
          _ -> showAsm dest
      in "set" ++ getFlagConditionSuffix flag ++ " " ++ destText


data OperandSize
  = OpSizeByte
  | OpSizeWord
  | OpSizeDword
  | OpSizeQword
  deriving (Show)


-- https://learn.microsoft.com/en-us/windows-hardware/drivers/debugger/x64-architecture
getAsmRegName :: OperandSize -> AsmReg -> String
getAsmRegName opSize reg = '%' : name
  where
    name = case reg of
      AsmRegAX -> xRegName "a"
      AsmRegBX -> xRegName "b"
      AsmRegCX -> xRegName "c"
      AsmRegDX -> xRegName "d"
      AsmRegDI -> regName "di"
      AsmRegSI -> regName "si"
      AsmRegBP -> regName "bp"
      AsmRegSP -> regName "sp"
      AsmRegR8 -> rRegName "r8"
      AsmRegR9 -> rRegName "r9"
      AsmRegR10 -> rRegName "r10"
      AsmRegR11 -> rRegName "r11"
      AsmRegR12 -> rRegName "r12"
      AsmRegR13 -> rRegName "r13"
      AsmRegR14 -> rRegName "r14"
      AsmRegR15 -> rRegName "r15"

    regName baseName = case opSize of
      OpSizeByte -> baseName ++ "l"
      OpSizeWord -> baseName
      OpSizeDword -> 'e' : baseName
      OpSizeQword -> 'r' : baseName

    xRegName baseName = case opSize of
      OpSizeByte -> baseName ++ "l"
      OpSizeWord -> baseName ++ "x"
      OpSizeDword -> 'e' : baseName ++ "x"
      OpSizeQword -> 'r' : baseName ++ "x"

    rRegName baseName = case opSize of
      OpSizeByte -> baseName ++ "b"
      OpSizeWord -> baseName ++ "w"
      OpSizeDword -> baseName ++ "d"
      OpSizeQword -> baseName




instance ShowAsm AsmReadOperand where
  showAsm (AsmReadRegister reg) = getAsmRegName OpSizeDword reg
  showAsm (AsmImmVal value) = '$' : show value
  showAsm (AsmReadPseudoRegister name) = showAsmPseudoRegister name
  showAsm (AsmReadStack offset) = showAsmStackOffset offset

instance ShowAsm AsmWriteOperand where
  showAsm (AsmWriteRegister reg) = getAsmRegName OpSizeDword reg
  showAsm (AsmWritePseudoRegister name) = showAsmPseudoRegister name
  showAsm (AsmWriteStack offset) = showAsmStackOffset offset


showAsmStackOffset :: StackOffset -> String
showAsmStackOffset offset = show (negate offset) ++ "(%rbp)"

showAsmPseudoRegister :: Identifier -> String
showAsmPseudoRegister name = "?(" ++ name ++ ")"


genAsmProgram :: IrProgram -> AsmProgram
genAsmProgram (IrProgram defs) = AsmProgram $ map genAsmDef defs


genAsmDef :: IrDefinition -> AsmDefinition
genAsmDef (IrFunctionDefinition name params instructions) =
  let
    funcInstructions = concatMap genAsmInstructions instructions
    paramInstructions = genFunctionParameterCopyInstructions params
    instructions' = paramInstructions ++ funcInstructions
    stackSize = 8 * getNumStackFuncParams params
  in
    AsmFuncDef name stackSize instructions'


genFunctionParameterCopyInstructions :: [FunctionParameter] -> [AsmInstruction]
genFunctionParameterCopyInstructions params = go paramSources
  where
    -- -- The stack arguments get pushed in reverse order.
    -- params' =
    --   let n = fromIntegral numRegisterFuncParams
    --   in take n params ++ reverse (drop n params)
    params' = params

    -- Read the param sources in reverse order so that we can read out
    -- stack-passed arguments before overwriting them with the contents
    -- of register-passed arguments.
    paramSources = reverse $ zip functionArgSources params'

    go :: [(FunctionArgSource, FunctionParameter)] -> [AsmInstruction]
    go [] = []
    go ((source, FunctionParameter name):xs) = case source of
      FuncArgRegister reg -> AsmMov (AsmReadRegister reg) argPseudoReg : go xs
      -- FUTURE: This just copies the function argument from one stack slot to another.
      -- Why not just use it in the stack slot it already has? Need to somehow tell
      -- the pseudo-reg allocator that it already has stack space.
      FuncArgStack offset -> AsmMov (AsmReadStack offset) argPseudoReg : go xs
      where
        argPseudoReg = AsmWritePseudoRegister name



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
  if left == readIrValue dest
    then instructions
    else AsmMov asmLeft asmDest : instructions
  where
    instructions = [AsmBinary asmOp asmRight asmDest]
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


genAsmInstructions (IrFunctionCall name args result) =
  pushRegisters
    . fixStackAlignment
    . passArguments
    . emitCall
    . restoreStack
    . popRegisters
    . getReturnValue $ []
  where

    pushedRegisters = map fst $ getRegFuncParams $ zip funcParamRegSources args

    pushRegisters :: [AsmInstruction] -> [AsmInstruction]
    pushRegisters = id  -- (map (AsmPush . AsmReadRegister) pushedRegisters ++)

    popRegisters :: [AsmInstruction] -> [AsmInstruction]
    popRegisters = id  -- (map (AsmPop . AsmWriteRegister) (reverse pushedRegisters) ++)

    -- If the number of stack arguments is odd, then we have to allocate
    -- some additional stack to align the stack pointer to 16 bytes.
    needToFixStackAlignment = odd $ getNumStackFuncParams args

    fixStackAlignment :: [AsmInstruction] -> [AsmInstruction]
    fixStackAlignment
      | needToFixStackAlignment = (AsmAllocateStack 8 :)
      | otherwise = id

    restoreStack :: [AsmInstruction] -> [AsmInstruction]
    restoreStack =
      let
        unfixStackAlignmentAmount = if needToFixStackAlignment then 8 else 0
        pushedRegistersAmount = 8 * getNumStackFuncParams args
        offset = unfixStackAlignmentAmount + pushedRegistersAmount
      in
        if offset == 0 then id else (AsmDeallocateStack offset :)

    passArguments :: [AsmInstruction] -> [AsmInstruction]
    passArguments = go $ zip functionArgSources args'
      where
        -- The stack arguments get pushed in reverse order.
        args' =
          let n = fromIntegral numRegisterFuncParams
          in take n args ++ reverse (drop n args)

        go :: [(FunctionArgSource, IrReadValue)] -> [AsmInstruction] -> [AsmInstruction]
        go [] xs = xs
        go ((FuncArgRegister reg, value) : rest) xs =
          AsmMov (genAsmReadOperand value) (AsmWriteRegister reg) : go rest xs

        -- TODO: See page 199 for commentary on having to copy an argument
        -- in memory to register AX before pushing it. I'm not sure how we
        -- can know if a value is in memory or a register yet. Wouldn't we
        -- have to do that in the pseudo-register rewrite phase?
        go ((FuncArgStack _, value) : rest) xs =
          AsmPush (genAsmReadOperand value) : go rest xs

        -- TODO: I'm not sure if this is working.
        -- Arguments 7 and 8 are not getting passed correctly, or something.
        -- Or being read correctly on the other end.
        -- They're not getting pushed in reverse order like they should,
        -- but that wouldn't cause the problem I'm seeing in this case.
        -- gcc and zcc give the same answer summing 6 args, but not 7 or 8.
        --
        -- It looks like it's using the wrong parameter's stack offset
        -- when it tries to use a true stack parameter, but it's hard to tell
        -- without seeing the pre-PseudoRegister-replacement ASM Program output.
        -- I should add that to the compiler's output to see what it's trying
        -- to do before it does that replacement.


    emitCall :: [AsmInstruction] -> [AsmInstruction]
    emitCall = (AsmCall name :)

    getReturnValue :: [AsmInstruction] -> [AsmInstruction]
    getReturnValue =
      let dest = genAsmWriteOperand result
      in (AsmMov (AsmReadRegister AsmRegAX) dest :)


data FunctionArgSource
  = FuncArgRegister AsmReg
  | FuncArgStack StackOffset
  deriving (Show)

-- TODO: Does this correctly get the stack args in reverse order, as it should?
functionArgSources :: [FunctionArgSource]
functionArgSources = regSources ++ stackSources
  where
    regSources = map FuncArgRegister funcParamRegSources
    stackSources = map (FuncArgStack . negate . (+ 16) . (* 8)) [0..]


funcParamRegSources :: [AsmReg]
funcParamRegSources = [AsmRegDI, AsmRegSI, AsmRegDX, AsmRegCX, AsmRegR8, AsmRegR9]


numRegisterFuncParams :: Integer
numRegisterFuncParams = genericLength funcParamRegSources


getNumStackFuncParams :: [a] -> Integer
getNumStackFuncParams params = genericLength params - getNumRegFuncParams params


getNumRegFuncParams :: [a] -> Integer
getNumRegFuncParams params = min numRegisterFuncParams (genericLength params)


getRegFuncParams :: [a] -> [a]
getRegFuncParams params = take (fromIntegral $ getNumRegFuncParams params) params



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


