
module Compiler.CodeGen.AssemblyX64.ReplacePseudoRegisters (
  replaceProgramPseudoRegisters,
)
where

import Control.Monad.State (StateT (..), get, put)
import Control.Monad.Identity (Identity (..))

import qualified Data.Map as Map
import Compiler.CodeGen.AssemblyX64.Base


replaceProgramPseudoRegisters :: AsmProgram -> AsmProgram
replaceProgramPseudoRegisters (AsmProgram defs) =
  AsmProgram $ map replaceDefPseudoRegisters defs


replaceDefPseudoRegisters :: AsmDefinition -> AsmDefinition
replaceDefPseudoRegisters (AsmFuncDef name origStackSize instructions) =
  let
    (instructions', stackSize') = replaceInstructionPseudoRegisters instructions
    stackSize = stackSize' + origStackSize
  in AsmFuncDef name stackSize instructions'


type PseudoRegReplacerState =
  ( Map.Map Identifier StackOffset  -- assigned offsets
  , StackOffset                     -- current max offset
  )

type PseudoRegReplacerT m = StateT PseudoRegReplacerState m
type PseudoRegReplacer = PseudoRegReplacerT Identity

replaceInstructionPseudoRegisters :: [AsmInstruction] -> ([AsmInstruction], StackOffset)
replaceInstructionPseudoRegisters instructions = (outputInstructions, stackSize - 4)
  where
    initState = (Map.empty, 4)
    (outputInstructions, (_, stackSize)) =
      runIdentity $ runStateT (replaceInstructionPseudoRegisters' instructions) initState

replaceInstructionPseudoRegisters' :: [AsmInstruction] -> PseudoRegReplacer [AsmInstruction]
replaceInstructionPseudoRegisters' [] = return []
replaceInstructionPseudoRegisters' (x:xs) = do
  newInstructions <- f x
  restInstructions <- replaceInstructionPseudoRegisters' xs
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

    f (AsmMov src@(AsmReadStack _) (AsmWritePseudoRegister destName)) = do
      newDest <- replaceWritePseudoReg destName
      return [AsmMov src (AsmWriteRegister AsmRegR10), AsmMov (AsmReadRegister AsmRegR10) newDest]

    f (AsmMov src (AsmWritePseudoRegister destName)) = do
      newDest <- replaceWritePseudoReg destName
      return [AsmMov src newDest]

    f (AsmMov (AsmReadStack src) (AsmWriteStack dest))
      | src == dest = return []
      | otherwise = return
        [ AsmMov (AsmReadStack src) (AsmWriteRegister AsmRegR10)
        , AsmMov (AsmReadRegister AsmRegR10) (AsmWriteStack dest)
        ]

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

