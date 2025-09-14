{-# LANGUAGE NoFieldSelectors #-}

module Compiler.SemanticAnalysis.VariableResolution (
  resolveVariables,
  VarResolverError,
)
where

import qualified Data.Map as Map
import Control.Monad.State (StateT (..), get, put)
import Control.Monad.Identity (Identity (..))
import Control.Monad.Writer (WriterT (..), MonadWriter (tell))

import Compiler.Parser.Parser
import Control.Monad (when, unless)


type Identifier = String

type VariableCounter = Integer
type Variables = Map.Map Identifier Identifier
type VarResolverState = (VariableCounter, Variables)

type VarResolverError = String
type VarResolver = VarResolverT Identity
type VarResolverT m = WriterT [VarResolverError] (StateT VarResolverState m)


resolveVariables :: FuncDef -> (FuncDef, [VarResolverError])
resolveVariables func = (func', errors)
  where
    initState = (0, Map.empty)
    ((func', errors), _) = runIdentity $ runStateT (runWriterT $ resolveVariables' func) initState


resolveVariables' :: FuncDef -> VarResolver FuncDef
resolveVariables' (FuncDef funcName blockItems) = do
  blockItems' <- go blockItems
  return $ FuncDef funcName blockItems'

  where
    go :: [BlockItem] -> VarResolver [BlockItem]
    go [] = return []

    go (BlockItemDeclaration decl : xs) = do
      decl' <- resolveDeclVars funcName decl
      let item = BlockItemDeclaration decl'
      (item :) <$> go xs

    go (BlockItemStatement stmt : xs) = do
      stmt' <- resolveStmtVars funcName stmt
      let item = BlockItemStatement stmt'
      (item :) <$> go xs


-- TODO: use a reader monad for e.g. the function name

resolveDeclVars :: Identifier -> Declaration -> VarResolver Declaration
resolveDeclVars funcName (VariableDeclaration varName initExpr) = do
  (uniqueVarName, isNewVarName) <- declareUniqueVarName funcName varName
  unless isNewVarName $ do
    let msg = "variable '" ++ varName ++ "' was already declared in function '" ++ funcName ++ "'"
    tell [msg]

  initExpr' <- traverse (resolveExprVars funcName) initExpr
  return $ VariableDeclaration uniqueVarName initExpr'


resolveStmtVars :: Identifier -> Statement -> VarResolver Statement
resolveStmtVars funcName (ReturnStatement expr) =
  ReturnStatement <$> resolveExprVars funcName expr

resolveStmtVars _ NullStatement = return NullStatement

resolveStmtVars funcName (ExpressionStatement expr) =
  ExpressionStatement <$> resolveExprVars funcName expr

resolveStmtVars funcName (CompoundStatement stmts) =
  CompoundStatement . reverse <$> go stmts
  where
    go :: [Statement] -> VarResolver [Statement]
    go [] = return []
    go (x:xs) = do
      x' <- resolveStmtVars funcName x
      (x' : ) <$> go xs

resolveStmtVars funcName (IfStatement expr stmt elseStmt) = do
  expr' <- resolveExprVars funcName expr
  stmt' <- resolveStmtVars funcName stmt
  elseStmt' <- traverse (resolveStmtVars funcName) elseStmt
  return $ IfStatement expr' stmt' elseStmt'


resolveExprVars :: Identifier -> Expression -> VarResolver Expression
resolveExprVars funcName (UnaryExpression op expr) =
  UnaryExpression op <$> resolveExprVars funcName expr
resolveExprVars funcName (BinaryExpression op left right) = do
  left' <- resolveExprVars funcName left
  right' <- resolveExprVars funcName right
  return $ BinaryExpression op left' right'

resolveExprVars funcName (VariableExpression name) = do
  (uniqueVarName, isNew) <- getUniqueVarName funcName name
  when isNew $ do
    let msg = "undeclared variable '" ++ name ++ "' in function '" ++ funcName ++ "'"
    tell [msg]
  return $ VariableExpression uniqueVarName

resolveExprVars funcName (ConditionalExpression cond ifTrue ifFalse) = do
  cond' <- resolveExprVars funcName cond
  ifTrue' <- resolveExprVars funcName ifTrue
  ifFalse' <- resolveExprVars funcName ifFalse
  return $ ConditionalExpression cond' ifTrue' ifFalse'

resolveExprVars _ expr@(ConstantExpression _) = return expr


-- TODO: emit errors in these functions if the name is missing or already exists?
getUniqueVarName :: Identifier -> Identifier -> VarResolver (Identifier, Bool)
getUniqueVarName = getUniqueVarNameImpl False

declareUniqueVarName :: Identifier -> Identifier -> VarResolver (Identifier, Bool)
declareUniqueVarName = getUniqueVarNameImpl True

getUniqueVarNameImpl :: Bool -> Identifier -> Identifier -> VarResolver (Identifier, Bool)
getUniqueVarNameImpl declare funcName varName = do
  (counter, vars) <- get
  case Map.lookup varName vars of
    Just uniqueVarName -> return (uniqueVarName, False)
    Nothing -> do
      let uniqueVarName = makeUniqueName counter
      let vars' = makeNewVars uniqueVarName vars
      put (counter + 1, vars')
      return (uniqueVarName, True)

  where
    makeUniqueName :: Integer -> Identifier
    makeUniqueName counter = "." ++ funcName ++ "." ++ varName ++ "." ++ show counter

    makeNewVars :: Identifier -> Variables -> Variables
    makeNewVars uniqueVarName vars = if declare
      then Map.insert varName uniqueVarName vars
      else vars

