
module Compiler.Parser.AstPrinting (
  printAst,
)
where

import Compiler.Parser.Parser

import Control.Monad.State (StateT (..))
import Control.Monad.Identity (Identity (..))
import Data.Maybe (isJust, fromMaybe)


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
  getAstNodeDescription (BlockItemStatement stmt) = getAstNodeDescription stmt
  getAstNodeDescription (BlockItemDeclaration decl) = getAstNodeDescription decl
  getAstNodeDescription (BlockItemLabel name) = name ++ ":"

  getAstSubnodeLines (BlockItemStatement decl) = getAstSubnodeLines decl
  getAstSubnodeLines (BlockItemDeclaration decl) = getAstSubnodeLines decl
  getAstSubnodeLines (BlockItemLabel _) = return []


instance AstPrintNode Declaration where
  getAstNodeDescription (VariableDeclaration name _) =
    "VariableDeclaration : int " ++ name
  getAstSubnodeLines (VariableDeclaration _ initExpr) =
    maybe (return []) printAstNode initExpr


instance AstPrintNode Statement where
  getAstNodeDescription NullStatement = "NullStatement"
  getAstNodeDescription (GotoStatement name) = "GotoStatement : '" ++ name ++ "'"
  getAstNodeDescription (ReturnStatement _) = "ReturnStatement"
  getAstNodeDescription (ExpressionStatement _) = "ExpressionStatement"
  getAstNodeDescription (CompoundStatement stmts) =
    "CompoundStatement (" ++ show (length stmts) ++ ")"
  getAstNodeDescription (IfStatement _ _ elseStmt) =
    "IfStatement" ++ (if isJust elseStmt then " (if/else)" else "")

  getAstSubnodeLines NullStatement = return []
  getAstSubnodeLines (GotoStatement _) = return []
  getAstSubnodeLines (ReturnStatement expr) = printAstNode expr
  getAstSubnodeLines (ExpressionStatement expr) = printAstNode expr
  getAstSubnodeLines (CompoundStatement stmts) = do
    concat <$> mapM printAstNode stmts
  getAstSubnodeLines (IfStatement expr stmt elseStmt) = do
    exprLines <- printAstNode expr
    stmtLines <- printAstNode stmt
    elseStmtLines <- traverse printAstNode elseStmt
    return $ concat [exprLines, stmtLines, fromMaybe [] elseStmtLines]


instance AstPrintNode Expression where
  getAstNodeDescription (UnaryExpression op _) = "UnaryExpression : " ++ show op
  getAstNodeDescription (BinaryExpression op _ _) = "BinaryExpression : " ++ show op
  getAstNodeDescription (VariableExpression name) = "VariableExpression : '" ++ name ++ "'"
  getAstNodeDescription (ConstantExpression value) = "ConstantExpression : " ++ show value
  getAstNodeDescription (ConditionalExpression {}) = "ConditionalExpression"

  getAstSubnodeLines (UnaryExpression _ expr) = printAstNode expr
  getAstSubnodeLines (BinaryExpression _ left right) = do
    leftLines <- printAstNode left
    rightLines <- printAstNode right
    return $ leftLines ++ rightLines
  getAstSubnodeLines (ConditionalExpression cond ifTrue ifFalse) = do
    condLines <- printAstNode cond
    ifTrueLines <- printAstNode ifTrue
    ifFalseLines <- printAstNode ifFalse
    return $ concat [condLines, ifTrueLines, ifFalseLines]
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

