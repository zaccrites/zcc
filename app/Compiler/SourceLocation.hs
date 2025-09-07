
module Compiler.SourceLocation (
  SourceLine,
  SourceColumn,
  SourceLocation (..),
  firstLocation,
  nextLineLocation,
  nextColumnLocation,
) where

type SourceLine = Int
type SourceColumn = Int
data SourceLocation = SourceLocation SourceLine SourceColumn

instance Show SourceLocation where
  show (SourceLocation line column) = show line ++ ":" ++ show column

firstLocation :: SourceLocation
firstLocation = SourceLocation 1 1

nextLineLocation :: SourceLocation -> SourceLocation
nextLineLocation (SourceLocation line _) = SourceLocation (line + 1) 1

nextColumnLocation :: SourceLocation -> SourceLocation
nextColumnLocation (SourceLocation line column) = SourceLocation line (column + 1)
