{-# LANGUAGE BlockArguments #-}

module Compiler.SemanticAnalysis.TypeChecking (
  checkTypes,
  TypeCheckerError,
)
where

import Compiler.Parser.Parser

import qualified Data.Map as Map
import Control.Monad.State (StateT (..), get, put, gets)
import Control.Monad.Identity (Identity (..))
import Control.Monad.Writer (WriterT (..), MonadWriter (tell))
import Control.Monad (when, unless)
import Data.Maybe (fromJust, fromMaybe)
import GHC.Generics (Datatype(isNewtype))

type ParamCount = Int

data Type
  = Int
  | Function ParamCount
  deriving (Show)

type Identifier = String
type SymbolTable = Map.Map Identifier Type

type TypeCheckerState = SymbolTable

type TypeCheckerError = String
type TypeChecker = TypeCheckerT Identity
type TypeCheckerT m = WriterT [TypeCheckerError] (StateT TypeCheckerState m)



checkTypes :: Program -> (Program, [TypeCheckerError])
checkTypes program = (program', errors)
  where
    initState = Map.empty
    ((program', errors), _) = runIdentity $ runStateT (runWriterT $ checkTypes' program) initState


checkTypes' :: Program -> TypeChecker Program
checkTypes' (Program decls) = Program <$> mapM checkDeclTypes decls


-- checkFuncTypes :: FunctionDefinition -> TypeChecker FunctionDefinition
-- checkFuncTypes (FunctionDefinition linkage params block) = do
--   FunctionDefinition linkage params <$> checkBlockTypes block


checkBlockTypes :: Block -> TypeChecker Block
checkBlockTypes = mapM checkBlockItemTypes


checkBlockItemTypes :: BlockItem -> TypeChecker BlockItem
checkBlockItemTypes (BlockItemDeclaration decl) = BlockItemDeclaration <$> checkDeclTypes decl
checkBlockItemTypes (BlockItemStatement stmt) = BlockItemStatement <$> checkStmtTypes stmt
checkBlockItemTypes item@(BlockItemLabel _) = return item


checkDeclTypes :: Declaration -> TypeChecker Declaration
checkDeclTypes (VariableDeclaration linkage name initExpr) = do
  let newTyp = Int
  checkExistingTypeCompatibility name newTyp
  addSymbol name newTyp
  initExpr' <- traverse checkExprTypes initExpr
  return $ VariableDeclaration linkage name initExpr'

checkDeclTypes (FunctionDeclaration linkage name params block) = do
  let newTyp = Function (length params)
  checkExistingTypeCompatibility name newTyp
  addSymbol name newTyp
  block' <- traverse checkBlockTypes block
  return $ FunctionDeclaration linkage name params block'


checkExistingTypeCompatibility :: Identifier -> Type -> TypeChecker ()
checkExistingTypeCompatibility name newTyp = do
  existingTyp <- maybeGetSymbolType name
  case existingTyp of
    Nothing -> return ()
    Just typ -> unless (isCompatibleType newTyp typ) do
      tell ["new declaration is incompatible with existing one"]



isCompatibleType :: Type -> Type -> Bool
isCompatibleType Int Int = True
isCompatibleType (Function count1) (Function count2) = count1 == count2
isCompatibleType _ _ = False

-- isSameType :: Type -> Type -> Bool
-- isSameType Int Int = True
-- isSameType (Function count1) (Function count2) = count1 == count2
-- isSameType _ _ = False

-- instance Eq Type where
--   Int == Int = True
--   (Function count1) == (Function count2) = count1 == count2
--   _ == _ = False




checkStmtTypes :: Statement -> TypeChecker Statement
checkStmtTypes stmt = return stmt


checkExprTypes :: Expression -> TypeChecker Expression
checkExprTypes expr@(FunctionCallExpression name args) = do
  typ <- getSymbolType name
  case typ of
    Function numParams ->
      when (length args /= numParams) do
        tell ["'" ++ name ++ "' expected " ++ show numParams ++ " arguments but was only given " ++ (show . length) args]
    _ -> tell ["'" ++ name ++ "' is not of function type"]
  return expr


checkExprTypes expr@(VariableExpression name) = do
  typ <- getSymbolType name
  when (isFunctionType typ) do
    tell ["'" ++ name ++ "' is of function type"]
  return expr

checkExprTypes expr = return expr


isFunctionType :: Type -> Bool
isFunctionType (Function _) = True
isFunctionType _ = False


addSymbol :: Identifier -> Type -> TypeChecker ()
addSymbol name typ = do
  table <- get
  let table' = Map.insert name typ table
  put table'


-- getSymbolType :: Identifier -> TypeChecker (Maybe Type)
-- getSymbolType name = gets (Map.lookup name)

getSymbolType :: Identifier -> TypeChecker Type
getSymbolType name = fromJust <$> maybeGetSymbolType name

maybeGetSymbolType :: Identifier -> TypeChecker (Maybe Type)
maybeGetSymbolType name = gets (Map.lookup name)
