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
    go (BlockItemDeclaration (VariableDeclaration varName initExpr):xs) = do
      (uniqueVarName, isNewVarName) <- declareUniqueVarName funcName varName
      unless isNewVarName $ do
        let msg = "variable '" ++ varName ++ "' was already declared in function '" ++ funcName ++ "'"
        tell [msg]

      initExpr' <- traverse (resolveExprVars funcName) initExpr
      let item = BlockItemDeclaration (VariableDeclaration uniqueVarName initExpr')
      (item :) <$> go xs

    go (BlockItemStatement (ReturnStatement expr):xs) = do
      expr' <- resolveExprVars funcName expr
      let item = BlockItemStatement (ReturnStatement expr')
      (item :) <$> go xs

    go (x@(BlockItemStatement NullStatement):xs) = (x :) <$> go xs

    go (BlockItemStatement (ExpressionStatement expr):xs) = do
      expr' <- resolveExprVars funcName expr
      let item = BlockItemStatement (ExpressionStatement expr')
      (item :) <$> go xs


-- TODO: use a reader monad for e.g. the function name

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

