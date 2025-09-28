
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
  getAstSubnodeLines (Program decls) = concat <$> mapM printAstNode decls


instance AstPrintNode Block where
  getAstNodeDescription _ = "Block"
  getAstSubnodeLines items = concat <$> mapM printAstNode items

instance AstPrintNode BlockItem where
  getAstNodeDescription (BlockItemStatement stmt) = getAstNodeDescription stmt
  getAstNodeDescription (BlockItemDeclaration decl) = getAstNodeDescription decl
  getAstNodeDescription (BlockItemLabel name) = name ++ ":"

  getAstSubnodeLines (BlockItemStatement decl) = getAstSubnodeLines decl
  getAstSubnodeLines (BlockItemDeclaration decl) = getAstSubnodeLines decl
  getAstSubnodeLines (BlockItemLabel _) = return []


instance AstPrintNode Declaration where
  getAstNodeDescription (VariableDeclaration linkage name _) =
    "VariableDeclaration : '" ++ name ++ "'"
  getAstNodeDescription (FunctionDeclaration linkage name params _) =
    let paramCount = makeCountString "param" "params" params
    in "FunctionDeclaration : '" ++ name ++ "' (" ++ paramCount ++ ")"

  getAstSubnodeLines (VariableDeclaration linkage _ initExpr) =
    maybe (return []) printAstNode initExpr

  getAstSubnodeLines (FunctionDeclaration _ _ params block) = do
    paramsLines <- concat <$> mapM printAstNode params
    blockLines <- fromMaybe [] <$> traverse printAstNode block
    return $ paramsLines ++ blockLines


instance AstPrintNode FunctionParameter where
  getAstNodeDescription (FunctionParameter name) = "FunctionParameter : '" ++ name ++ "'"


makeCountString :: String -> String -> [a] -> String
makeCountString singular _ [_] = "1 " ++ singular
makeCountString _ plural items = let count = show . length $ items in count ++ " " ++ plural


instance AstPrintNode Statement where
  getAstNodeDescription NullStatement = "NullStatement"
  getAstNodeDescription (GotoStatement name) = "GotoStatement : '" ++ name ++ "'"
  getAstNodeDescription (ReturnStatement _) = "ReturnStatement"
  getAstNodeDescription (ExpressionStatement _) = "ExpressionStatement"
  getAstNodeDescription (CompoundStatement stmts) =
    "CompoundStatement (" ++ show (length stmts) ++ ")"
  getAstNodeDescription (IfStatement _ _ elseStmt) =
    "IfStatement" ++ (if isJust elseStmt then " (if/else)" else "")

  getAstNodeDescription (ContinueStatement (LoopLabel "")) = "ContinueStatement"
  getAstNodeDescription (ContinueStatement (LoopLabel label)) = "ContinueStatement : '" ++ label ++ "'"
  getAstNodeDescription (WhileStatement _ _ (LoopLabel "")) = "WhileStatement"
  getAstNodeDescription (WhileStatement _ _ (LoopLabel label)) = "WhileStatement : '" ++ label ++ "'"
  getAstNodeDescription (DoWhileStatement _ _ (LoopLabel "")) = "DoWhileStatement"
  getAstNodeDescription (DoWhileStatement _ _ (LoopLabel label)) = "DoWhileStatement : '" ++ label ++ "'"
  getAstNodeDescription (ForStatement _ _ _ _ (LoopLabel "")) = "ForStatement"
  getAstNodeDescription (ForStatement _ _ _ _ (LoopLabel label)) = "ForStatement : '" ++ label ++ "'"
  getAstNodeDescription (SwitchStatement _ _ (SwitchLabel "")) = "SwitchStatement"
  getAstNodeDescription (SwitchStatement _ _ (SwitchLabel label)) = "SwitchStatement : '" ++ label ++ "'"
  getAstNodeDescription (BreakStatement target) =
    "BreakStatement" ++ suffix
    where
      suffix = case target of
        BreakTargetLoop (LoopLabel "") -> ""
        BreakTargetLoop (LoopLabel label) -> " : loop '" ++ label ++ "'"
        BreakTargetSwitch (SwitchLabel "") -> ""
        BreakTargetSwitch (SwitchLabel label) -> " : switch '" ++ label ++ "'"


  getAstSubnodeLines NullStatement = return []
  getAstSubnodeLines (GotoStatement _) = return []
  getAstSubnodeLines (ContinueStatement _) = return []
  getAstSubnodeLines (BreakStatement _) = return []
  getAstSubnodeLines (ReturnStatement expr) = printAstNode expr
  getAstSubnodeLines (ExpressionStatement expr) = printAstNode expr
  getAstSubnodeLines (CompoundStatement stmts) = do
    concat <$> mapM printAstNode stmts

  getAstSubnodeLines (IfStatement expr stmt elseStmt) = do
    exprLines <- printAstNode expr
    stmtLines <- printAstNode stmt
    elseStmtLines <- traverse printAstNode elseStmt
    return $ concat [exprLines, stmtLines, fromMaybe [] elseStmtLines]

  getAstSubnodeLines (WhileStatement expr stmt _) = do
    exprLines <- printAstNode expr
    stmtLines <- printAstNode stmt
    return $ exprLines ++ stmtLines
  getAstSubnodeLines (DoWhileStatement expr stmt _) = do
    exprLines <- printAstNode expr
    stmtLines <- printAstNode stmt
    return $ exprLines ++ stmtLines
  getAstSubnodeLines (ForStatement forInit expr forPost stmt _) = do
    initLines <- printAstNode forInit
    exprLines <- traverse printAstNode expr
    forPostLines <- traverse printAstNode forPost
    stmtLines <- printAstNode stmt
    return (
      initLines ++
      fromMaybe ["(empty condition expression)"] exprLines ++
      fromMaybe ["(empty post expression)"] forPostLines ++
      stmtLines
      )

  getAstSubnodeLines (SwitchStatement expr items _) = do
    exprLines <- printAstNode expr
    itemLines <- concat <$> mapM printAstNode items
    return $ exprLines ++ itemLines


instance AstPrintNode SwitchItem where

  getAstNodeDescription (SwitchItemCase value "") = "SwitchItemCase " ++ show value
  getAstNodeDescription (SwitchItemCase value label) = "SwitchItemCase " ++ show value ++ " : '" ++ label ++ "'"
  getAstNodeDescription (SwitchItemDefaultCase "") = "SwitchItemDefaultCase"
  getAstNodeDescription (SwitchItemDefaultCase label) = "SwitchItemDefaultCase : '" ++ label ++ "'"
  getAstNodeDescription (SwitchItemStatement _) = "SwitchItemStatement"

  getAstSubnodeLines (SwitchItemCase _ _) = return []
  getAstSubnodeLines (SwitchItemDefaultCase _) = return []
  getAstSubnodeLines (SwitchItemStatement stmt) = printAstNode stmt


instance AstPrintNode ForInit where
  getAstNodeDescription (ForInitDeclaration decl) = getAstNodeDescription decl
  getAstNodeDescription (ForInitExpression expr) = getAstNodeDescription expr
  getAstNodeDescription ForInitEmpty = "(empty for init)"

  getAstSubnodeLines (ForInitDeclaration decl) = getAstSubnodeLines decl
  getAstSubnodeLines (ForInitExpression expr) = getAstSubnodeLines expr
  getAstSubnodeLines ForInitEmpty = return []


instance AstPrintNode Expression where
  getAstNodeDescription (UnaryExpression op _) = "UnaryExpression : " ++ show op
  getAstNodeDescription (BinaryExpression op _ _) = "BinaryExpression : " ++ show op
  getAstNodeDescription (VariableExpression name) = "VariableExpression : '" ++ name ++ "'"
  getAstNodeDescription (ConstantExpression value) = "ConstantExpression : " ++ show value
  getAstNodeDescription (ConditionalExpression {}) = "ConditionalExpression"
  getAstNodeDescription (FunctionCallExpression name _) = "FunctionCallExpression : '" ++ name ++ "'"

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
  getAstSubnodeLines (FunctionCallExpression _ args) = concat <$> mapM printAstNode args
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

