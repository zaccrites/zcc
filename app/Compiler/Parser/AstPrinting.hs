
module Compiler.Parser.AstPrinting (
  printAst,
)
where

import Compiler.Parser.Parser

import Control.Monad.State (StateT (..), get, put, modify)
import Control.Monad.Identity (Identity (..))
import Control.Monad (when)


type IndentLevel = Int
type AstPrinterState = IndentLevel

type AstPrinterT m = StateT AstPrinterState m
type AstPrinter = AstPrinterT Identity


class AstPrintNode a where
  getAstNodeDescription :: a -> String
  getAstSubnodeLines :: a -> AstPrinter [String]
  getAstSubnodeLines _ = return []


instance AstPrintNode Program where
  getAstNodeDescription (Program _) = "Program"
  getAstSubnodeLines (Program func) = printAstNode func


instance AstPrintNode FuncDef where
  getAstNodeDescription (FuncDef name _) = "Function : int " ++ name
  getAstSubnodeLines (FuncDef _ items) = concat <$> mapM printAstNode items


instance AstPrintNode BlockItem where
  getAstNodeDescription (BlockItemDeclaration (VariableDeclaration name _)) =
    "VariableDeclaration : int " ++ name
  getAstNodeDescription (BlockItemStatement NullStatement) = "NullStatement"
  getAstNodeDescription (BlockItemStatement (ReturnStatement _)) = "ReturnStatement"
  getAstNodeDescription (BlockItemStatement (ExpressionStatement _)) = "ExpressionStatement"

  getAstSubnodeLines (BlockItemStatement NullStatement) = return []
  getAstSubnodeLines (BlockItemStatement (ReturnStatement expr)) = printAstNode expr
  getAstSubnodeLines (BlockItemStatement (ExpressionStatement expr)) = printAstNode expr
  getAstSubnodeLines (BlockItemDeclaration (VariableDeclaration _ initExpr)) =
    maybe (return []) printAstNode initExpr


instance AstPrintNode Expression where
  getAstNodeDescription (UnaryExpression op _) = "UnaryExpression : " ++ show op
  getAstNodeDescription (BinaryExpression op _ _) = "BinaryExpression : " ++ show op
  getAstNodeDescription (VariableExpression name) = "VariableExpression : '" ++ name ++ "'"
  getAstNodeDescription (AssignmentExpression _ _) = "AssignmentExpression"
  getAstNodeDescription x = show x

  getAstSubnodeLines (AssignmentExpression left right) = do
    leftLines <- printAstNode left
    rightLines <- printAstNode right
    return $ leftLines ++ rightLines
  getAstSubnodeLines (UnaryExpression _ expr) = printAstNode expr
  getAstSubnodeLines (BinaryExpression _ left right) = do
    leftLines <- printAstNode left
    rightLines <- printAstNode right
    return $ leftLines ++ rightLines
  getAstSubnodeLines _ = return []


printAst :: AstPrintNode a => a -> [String]
printAst node =
  let (output, _) = runIdentity $ runStateT (printAstNode node) 0
  in output


printAstNode :: AstPrintNode a => a -> AstPrinter [String]
printAstNode node = do
  let line = getAstNodeDescription node
  restLines <- map ("  "++) <$> getAstSubnodeLines node
  return $ makeResult line restLines
  where
    makeResult :: String -> [String] -> [String]
    makeResult line [] = [line]
    makeResult line restLines = [line ++ " ("] ++ restLines ++ [")"]


-- increaseIndent :: AstPrinter ()
-- increaseIndent = modify (+1)
--
-- decreaseIndent :: AstPrinter ()
-- decreaseIndent = modify (\level -> max 0 (level - 1))

-- indentString :: String -> AstPrinter String
-- indentString xs = do
--   level <- get
--   -- let indent = concat . replicate level $ ">"
--   let indent = "(" ++ show level ++ ")"
--   return $ indent ++ xs

