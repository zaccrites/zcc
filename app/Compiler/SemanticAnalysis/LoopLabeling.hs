
module Compiler.SemanticAnalysis.LoopLabeling (
  labelLoops,
  LoopLabelerError,
)
where

import Control.Monad.State (StateT (..), get, put)
import Control.Monad.Identity (Identity (..))
import Control.Monad.Writer (WriterT (..), MonadWriter (tell))

import Compiler.Parser.Parser


type LoopCounter = Integer
type LoopId = Integer
type LoopIdStack = [LoopId]
type LoopLabelerState = (LoopCounter, LoopIdStack)

type LoopLabelerError = String
type LoopLabeler = LoopLabelerT Identity
type LoopLabelerT m = WriterT [LoopLabelerError] (StateT LoopLabelerState m)


labelLoops :: Program -> (Program, [LoopLabelerError])
labelLoops program = (program', errors)
  where
    initState = (0, [])
    ((program', errors), _) = runIdentity $ runStateT (runWriterT $ labelLoops' program) initState


labelLoops' :: Program -> LoopLabeler Program
labelLoops' (Program func) = Program <$> labelFuncLoops func


labelFuncLoops :: FuncDef -> LoopLabeler FuncDef
labelFuncLoops (FuncDef name block) = FuncDef name <$> labelBlockLoops block


labelBlockLoops :: Block -> LoopLabeler Block
labelBlockLoops = mapM labelBlockItemLoops


labelBlockItemLoops :: BlockItem -> LoopLabeler BlockItem
labelBlockItemLoops item@(BlockItemDeclaration _) = return item
labelBlockItemLoops item@(BlockItemLabel _) = return item
labelBlockItemLoops (BlockItemStatement stmt) = BlockItemStatement <$> labelStatementLoops stmt


labelStatementLoops :: Statement -> LoopLabeler Statement
labelStatementLoops (WhileStatement expr stmt _) = do
  loopId <- pushLoopId
  stmt' <- labelStatementLoops stmt
  _ <- popLoopId
  let label = makeLoopLabel loopId
  return $ WhileStatement expr stmt' label

labelStatementLoops (DoWhileStatement expr stmt _) = do
  loopId <- pushLoopId
  stmt' <- labelStatementLoops stmt
  _ <- popLoopId
  let label = makeLoopLabel loopId
  return $ DoWhileStatement expr stmt' label


labelStatementLoops (ForStatement forInit expr forPost stmt _) = do
  loopId <- pushLoopId
  stmt' <- labelStatementLoops stmt
  _ <- popLoopId
  let label = makeLoopLabel loopId
  return $ ForStatement forInit expr forPost stmt' label


labelStatementLoops (ContinueStatement _) = do
  loopId <- peekLoopId
  label <- case loopId of
    Just loopId' -> return $ makeLoopLabel loopId'
    Nothing -> do
      tell ["'continue' statement outside of a loop!"]
      return "ERR"
  return $ ContinueStatement label


labelStatementLoops (BreakStatement _) = do
  loopId <- peekLoopId
  label <- case loopId of
    Just loopId' -> return $ makeLoopLabel loopId'
    Nothing -> do
      tell ["'continue' statement outside of a loop!"]
      return "ERR"
  return $ BreakStatement label


labelStatementLoops (IfStatement expr stmt elseStmt) = do
  stmt' <- labelStatementLoops stmt
  elseStmt' <- traverse labelStatementLoops elseStmt
  return $ IfStatement expr stmt' elseStmt'

labelStatementLoops (CompoundStatement block) =
  CompoundStatement <$> labelBlockLoops block

labelStatementLoops stmt@NullStatement = return stmt
labelStatementLoops stmt@(ReturnStatement _) = return stmt
labelStatementLoops stmt@(GotoStatement _) = return stmt
labelStatementLoops stmt@(ExpressionStatement _) = return stmt


makeLoopLabel :: LoopId -> String
makeLoopLabel loopId = "loop" ++ show loopId


pushLoopId :: LoopLabeler LoopId
pushLoopId = do
  (counter, stack) <- get
  let newLoopId = counter + 1
  put (newLoopId, newLoopId : stack)
  return newLoopId


popLoopId :: LoopLabeler (Maybe LoopId)
popLoopId = do
  (counter, stack) <- get
  case stack of
    (loopId : stack') -> do
      put (counter, stack')
      return $ Just loopId
    [] -> return Nothing


peekLoopId :: LoopLabeler (Maybe LoopId)
peekLoopId = do
  (_, stack) <- get
  case stack of
    (loopId : _) -> return $ Just loopId
    [] -> return Nothing

