
module Compiler.CodeGen.AssemblyX64.FixFunctionStackSizes (
  fixFunctionStackSizes,
)
where

import Compiler.CodeGen.AssemblyX64.Base
import Compiler.CodeGen.AssemblyX64.Utils


fixFunctionStackSizes :: AsmProgram -> AsmProgram
fixFunctionStackSizes (AsmProgram defs) = AsmProgram $ map fixDefStackSize defs


fixDefStackSize :: AsmDefinition -> AsmDefinition
fixDefStackSize (AsmFuncDef name stackSize instructions) =
  let stackSize' = roundUpStackSize stackSize
  in AsmFuncDef name stackSize' instructions

