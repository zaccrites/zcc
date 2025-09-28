{-# LANGUAGE NoFieldSelectors #-}

module Compiler.SemanticAnalysis.IdentifierResolution (
  resolveIdentifiers,
  ResolverError,
)
where

import qualified Data.Map as Map
import Control.Monad.State (StateT (..), get, put)
import Control.Monad.Identity (Identity (..))
import Control.Monad.Writer (WriterT (..), MonadWriter (tell))

import Compiler.Parser.Parser
import Control.Monad (when)


type Identifier = String

type VariableCounter = Integer

type Scope = Map.Map Identifier Identifier

type ScopeStack = [Scope]
type ResolverState = (VariableCounter, ScopeStack)

type ResolverError = String
type Resolver = ResolverT Identity
type ResolverT m = WriterT [ResolverError] (StateT ResolverState m)


resolveIdentifiers :: Program -> (Program, [ResolverError])
resolveIdentifiers program = (program', errors)
  where
    initState = (0, [Map.empty])
    ((program', errors), _) = runIdentity $ runStateT (runWriterT $ resolveVariables' program) initState


resolveVariables' :: Program -> Resolver Program
resolveVariables' (Program funcs) = Program <$> mapM resolveDeclVars funcs


-- resolveFuncVars :: FunctionDefinition -> Resolver FunctionDefinition
-- resolveFuncVars (FunctionDefinition name args block) = do
--   name' <- declareIdentifierName name
--   pushScope
--   args' <- mapM declareUniqueFunctionParamIdentifier args
--   block' <- resolveBlockVars False name block
--   popScope
--   return $ FunctionDefinition name' args' block'
--   where
--     declareUniqueFunctionParamIdentifier :: FunctionParameter -> Resolver FunctionParameter
--     declareUniqueFunctionParamIdentifier (FunctionParameter paramName) =
--       FunctionParameter <$> declareUniqueIdentifierName paramName

-- TODO: use a reader monad for e.g. the function name

resolveDeclVars :: Declaration -> Resolver Declaration
resolveDeclVars (VariableDeclaration linkage name initExpr) = do
  name' <- declareIdentifierWithLinkage linkage name
  initExpr' <- traverse resolveExprVars initExpr
  return $ VariableDeclaration linkage name' initExpr'

resolveDeclVars (FunctionDeclaration linkage name params (Just block)) = do
  name' <- declareIdentifierWithLinkage linkage name
  pushScope
  params' <- mapM declareFunctionParamIdentifier params
  block' <- resolveBlockVars False block
  popScope
  return $ FunctionDeclaration linkage name' params' (Just block')
  where
    declareFunctionParamIdentifier :: FunctionParameter -> Resolver FunctionParameter
    declareFunctionParamIdentifier (FunctionParameter paramName) =
      FunctionParameter <$> declareUniqueIdentifierName paramName

resolveDeclVars (FunctionDeclaration linkage name params Nothing) = do
  name' <- declareIdentifierWithLinkage linkage name
  return $ FunctionDeclaration linkage name' params Nothing



resolveBlockVars :: Bool -> Block -> Resolver Block
resolveBlockVars newScope block = do
  when newScope pushScope
  items <- mapM resolveBlockItemVars block
  when newScope popScope
  return items


resolveBlockItemVars :: BlockItem -> Resolver BlockItem
resolveBlockItemVars item@(BlockItemLabel _) = return item

resolveBlockItemVars (BlockItemDeclaration decl) =
  BlockItemDeclaration <$> resolveDeclVars decl

resolveBlockItemVars (BlockItemStatement stmt) =
  BlockItemStatement <$> resolveStmtVars stmt


resolveStmtVars :: Statement -> Resolver Statement
resolveStmtVars stmt@NullStatement = return stmt
resolveStmtVars stmt@(GotoStatement _) = return stmt

resolveStmtVars (ReturnStatement expr) =
  ReturnStatement <$> resolveExprVars expr

resolveStmtVars (ExpressionStatement expr) =
  ExpressionStatement <$> resolveExprVars expr

resolveStmtVars (CompoundStatement block) =
  CompoundStatement <$> resolveBlockVars True block

resolveStmtVars (IfStatement expr stmt elseStmt) = do
  expr' <- resolveExprVars expr
  stmt' <- resolveStmtVars stmt
  elseStmt' <- traverse resolveStmtVars elseStmt
  return $ IfStatement expr' stmt' elseStmt'


resolveStmtVars (WhileStatement expr stmt label) = do
  expr' <- resolveExprVars expr
  stmt' <- resolveStmtVars stmt
  return $ WhileStatement expr' stmt' label

resolveStmtVars (DoWhileStatement expr stmt label) = do
  expr' <- resolveExprVars expr
  stmt' <- resolveStmtVars stmt
  return $ DoWhileStatement expr' stmt' label

resolveStmtVars (ForStatement forInit expr forPost stmt label) = do
  pushScope  -- since we can declare a variable in the forInit
  forInit' <- resolveForInitVars forInit
  expr' <- traverse resolveExprVars expr
  forPost' <- traverse resolveExprVars forPost
  stmt' <- resolveStmtVars stmt
  popScope
  return $ ForStatement forInit' expr' forPost' stmt' label

resolveStmtVars stmt@(ContinueStatement _) = return stmt
resolveStmtVars stmt@(BreakStatement _) = return stmt

resolveStmtVars (SwitchStatement expr items label) = do
  expr' <- resolveExprVars expr
  items' <- mapM resolveSwitchItemVars items
  return $ SwitchStatement expr' items' label


resolveSwitchItemVars :: SwitchItem -> Resolver SwitchItem
resolveSwitchItemVars item@(SwitchItemCase _ _) = return item
resolveSwitchItemVars item@(SwitchItemDefaultCase _) = return item
resolveSwitchItemVars (SwitchItemStatement stmt) =
  SwitchItemStatement <$> resolveStmtVars stmt


resolveForInitVars :: ForInit -> Resolver ForInit
resolveForInitVars ForInitEmpty = return ForInitEmpty
resolveForInitVars (ForInitExpression expr) = ForInitExpression <$> resolveExprVars expr
resolveForInitVars (ForInitDeclaration decl) = ForInitDeclaration <$> resolveDeclVars decl


resolveExprVars :: Expression -> Resolver Expression
resolveExprVars (UnaryExpression op expr) =
  UnaryExpression op <$> resolveExprVars expr

resolveExprVars (BinaryExpression op left right) = do
  left' <- resolveExprVars left
  right' <- resolveExprVars right
  return $ BinaryExpression op left' right'

resolveExprVars (VariableExpression name) = do
  uniqueVarName <- getIdentifierName name
  return $ VariableExpression uniqueVarName

resolveExprVars (ConditionalExpression cond ifTrue ifFalse) = do
  cond' <- resolveExprVars cond
  ifTrue' <- resolveExprVars ifTrue
  ifFalse' <- resolveExprVars ifFalse
  return $ ConditionalExpression cond' ifTrue' ifFalse'

resolveExprVars expr@(ConstantExpression _) = return expr

resolveExprVars (FunctionCallExpression name args) = do
  name' <- getIdentifierName name
  FunctionCallExpression name' <$> mapM resolveExprVars args


pushScope :: Resolver ()
pushScope = do
  (counter, stack) <- get
  let stack' = Map.empty : stack
  put (counter, stack')


popScope :: Resolver ()
popScope = do
  (counter, stack) <- get
  case stack of
    (_ : stack') -> put (counter, stack')
    [] -> put (counter, stack)


-- TODO: Use ExceptT and just emit some kind of (VariableAlreadyExists Identifier) error type.

getIdentifierName :: Identifier -> Resolver Identifier
getIdentifierName name = do
  (_, stack) <- get
  go stack
  where
    go :: ScopeStack -> Resolver Identifier
    go [] = do
      -- Just make something up and emit an error.
      tell ["no variable '" ++ name ++ "' in scope"]
      declareUniqueIdentifierName name
    go (vars : stack) = case Map.lookup name vars of
      Just uniqueName -> return uniqueName
      Nothing -> go stack






declareIdentifierWithLinkage :: Linkage -> Identifier -> Resolver Identifier
declareIdentifierWithLinkage NoLinkage = declareUniqueIdentifierName
declareIdentifierWithLinkage ExternalLinkage = declareIdentifierName
declareIdentifierWithLinkage InternalLinkage = declareIdentifierName


declareIdentifierName :: Identifier -> Resolver Identifier
declareIdentifierName = declareIdentifierName' True return


declareUniqueIdentifierName :: Identifier -> Resolver Identifier
declareUniqueIdentifierName = declareIdentifierName' False makeUniqueIdentifierName

declareIdentifierName' :: Bool -> (Identifier -> Resolver Identifier) -> Identifier -> Resolver Identifier
declareIdentifierName' duplicatesAllowed nameMaker name = do
  (_, stack) <- get
  case stack of
    (scope : _) -> case Map.lookup name scope of
      Just name' -> if duplicatesAllowed
        then return name'
        else emitError name'
      Nothing -> insertNewName scope
    [] -> error "empty stack, should not be possible"

  where
    insertNewName :: Scope -> Resolver Identifier
    insertNewName scope = do
      name' <- nameMaker name
      let scope' = Map.insert name name' scope
      (counter, stack) <- get
      case stack of
        _ : stack' -> put (counter, scope' : stack')
        [] -> error "empty stack??"
      return name'

    emitError :: Identifier -> Resolver Identifier
    emitError existingName = do
      tell ["an identifier named '" ++ name ++ "' already exists"]
      return existingName


makeUniqueIdentifierName :: Identifier -> Resolver Identifier
makeUniqueIdentifierName name = do
  (counter, stack) <- get
  let name' = "." ++ name ++ "." ++ show counter
  put (counter + 1, stack)
  return name'

