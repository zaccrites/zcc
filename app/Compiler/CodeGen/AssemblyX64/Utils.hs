
module Compiler.CodeGen.AssemblyX64.Utils (
  roundUpStackSize,
)
where

import Compiler.CodeGen.AssemblyX64.Types


-- Round up a proposed stack offset to the nearest multiple of 16
-- to maintain alignment.
roundUpStackSize :: StackOffset -> StackOffset
roundUpStackSize size
  | remainder == 0 = size
  | otherwise = size + target - remainder
  where
    remainder = size `mod` target
    target = 16

