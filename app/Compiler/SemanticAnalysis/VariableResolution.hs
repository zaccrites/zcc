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


type Identifier = String

type VariableCounter = Integer
type Scope = Map.Map Identifier Identifier
type ScopeStack = [Scope]
type VarResolverState = (VariableCounter, ScopeStack)

type VarResolverError = String
type VarResolver = VarResolverT Identity
type VarResolverT m = WriterT [VarResolverError] (StateT VarResolverState m)


resolveVariables :: FuncDef -> (FuncDef, [VarResolverError])
resolveVariables func = (func', errors)
  where
    initState = (0, [])
    ((func', errors), _) = runIdentity $ runStateT (runWriterT $ resolveVariables' func) initState


resolveVariables' :: FuncDef -> VarResolver FuncDef
resolveVariables' (FuncDef funcName block) =
  FuncDef funcName <$> resolveBlockVars funcName block

-- TODO: use a reader monad for e.g. the function name

resolveDeclVars :: Identifier -> Declaration -> VarResolver Declaration
resolveDeclVars funcName (VariableDeclaration varName initExpr) = do
  uniqueVarName <- declareUniqueVarName funcName varName
  initExpr' <- traverse (resolveExprVars funcName) initExpr
  return $ VariableDeclaration uniqueVarName initExpr'


resolveBlockVars :: Identifier -> Block -> VarResolver Block
resolveBlockVars funcName block = do
  pushScopeStack
  items <- mapM (resolveBlockItemVars funcName) block
  popScopeStack
  return items


resolveBlockItemVars :: Identifier -> BlockItem -> VarResolver BlockItem
resolveBlockItemVars _ item@(BlockItemLabel _) = return item

resolveBlockItemVars funcName (BlockItemDeclaration decl) = case decl of
  VariableDeclaration _ _ -> BlockItemDeclaration <$> resolveDeclVars funcName decl

resolveBlockItemVars funcName (BlockItemStatement stmt) =
  BlockItemStatement <$> resolveStmtVars funcName stmt


resolveStmtVars :: Identifier -> Statement -> VarResolver Statement
resolveStmtVars _ stmt@NullStatement = return stmt
resolveStmtVars _ stmt@(GotoStatement _) = return stmt

resolveStmtVars funcName (ReturnStatement expr) =
  ReturnStatement <$> resolveExprVars funcName expr


resolveStmtVars funcName (ExpressionStatement expr) =
  ExpressionStatement <$> resolveExprVars funcName expr

resolveStmtVars funcName (CompoundStatement block) =
  CompoundStatement <$> resolveBlockVars funcName block

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
  uniqueVarName <- getUniqueVarName funcName name
  return $ VariableExpression uniqueVarName

resolveExprVars funcName (ConditionalExpression cond ifTrue ifFalse) = do
  cond' <- resolveExprVars funcName cond
  ifTrue' <- resolveExprVars funcName ifTrue
  ifFalse' <- resolveExprVars funcName ifFalse
  return $ ConditionalExpression cond' ifTrue' ifFalse'

resolveExprVars _ expr@(ConstantExpression _) = return expr


pushScopeStack :: VarResolver ()
pushScopeStack = do
  (counter, stack) <- get
  let stack' = Map.empty : stack
  put (counter, stack')


popScopeStack :: VarResolver ()
popScopeStack = do
  (counter, stack) <- get
  case stack of
    (_ : stack') -> put (counter, stack')
    [] -> put (counter, stack)


-- TODO: Use ExceptT and just emit some kind of (VariableAlreadyExists Identifier) error type.

getUniqueVarName :: Identifier -> Identifier -> VarResolver Identifier
getUniqueVarName funcName varName = do
  (_, stack) <- get
  go stack
  where
    go :: ScopeStack -> VarResolver Identifier
    go [] = do
      -- Just make something up and emit an error.
      tell ["no variable '" ++ varName ++ "' in scope in function '" ++ funcName ++ "'"]
      makeUniqueVarName varName
    go (vars : stack) = case Map.lookup varName vars of
      Just uniqueVarName -> return uniqueVarName
      Nothing -> go stack


declareUniqueVarName :: Identifier -> Identifier -> VarResolver Identifier
declareUniqueVarName funcName varName = do
  (_, stack) <- get
  case stack of
    (vars : _) -> case Map.lookup varName vars of
      Just existingUniqueVarName -> do
        tell ["a variable named '" ++ varName ++ "' already exists in scope in function '" ++ funcName ++ "'"]
        return existingUniqueVarName
      Nothing -> makeUniqueVarName varName
    [] -> error "empty stack, should not be possible"


makeUniqueVarName :: Identifier -> VarResolver Identifier
makeUniqueVarName varName = do
  (counter, stack) <- get
  case stack of
    (vars : stack') -> do
      let uniqueVarName = "." ++ varName ++ "." ++ show counter
      let vars' = Map.insert varName uniqueVarName vars
      put (counter + 1, vars' : stack')
      return uniqueVarName
    [] -> error "empty stack, should not be possible"

