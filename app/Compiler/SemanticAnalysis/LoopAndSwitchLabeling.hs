
module Compiler.SemanticAnalysis.LoopAndSwitchLabeling (
  labelLoopsAndSwitches,
  LabelerError,
)
where

import Control.Monad.State (StateT (..), get, put)
import Control.Monad.Identity (Identity (..))
import Control.Monad.Writer (WriterT (..), MonadWriter (tell))

import Compiler.Parser.Parser



type Counter = Integer


type LoopId = Counter
type SwitchId = Counter

type IdStack = [IdStackEntry]
data IdStackEntry
  = IdStackLoopEntry LoopId
  | IdStackSwitchEntry SwitchId


type LabelerState = (Counter, IdStack)

type LabelerError = String
type Labeler = LabelerT Identity
type LabelerT m = WriterT [LabelerError] (StateT LabelerState m)


labelLoopsAndSwitches :: Program -> (Program, [LabelerError])
labelLoopsAndSwitches program = (program', errors)
  where
    initState = (0, [])
    ((program', errors), _) = runIdentity $ runStateT (runWriterT $ labelLoopsAndSwitches' program) initState


labelLoopsAndSwitches' :: Program -> Labeler Program
labelLoopsAndSwitches' (Program func) = Program <$> labelFuncLoops func


labelFuncLoops :: FuncDef -> Labeler FuncDef
labelFuncLoops (FuncDef name block) = FuncDef name <$> labelBlockLoops block


labelBlockLoops :: Block -> Labeler Block
labelBlockLoops = mapM labelBlockItemLoops


labelBlockItemLoops :: BlockItem -> Labeler BlockItem
labelBlockItemLoops item@(BlockItemDeclaration _) = return item
labelBlockItemLoops item@(BlockItemLabel _) = return item
labelBlockItemLoops (BlockItemStatement stmt) = BlockItemStatement <$> labelStatementLoops stmt


labelStatementLoops :: Statement -> Labeler Statement
labelStatementLoops (WhileStatement expr stmt _) = do
  label <- makeLoopLabel <$> pushLoop
  stmt' <- labelStatementLoops stmt
  _ <- popStack
  return $ WhileStatement expr stmt' label

labelStatementLoops (DoWhileStatement expr stmt _) = do
  label <- makeLoopLabel <$> pushLoop
  stmt' <- labelStatementLoops stmt
  _ <- popStack
  return $ DoWhileStatement expr stmt' label


labelStatementLoops (ForStatement forInit expr forPost stmt _) = do
  label <- makeLoopLabel <$> pushLoop
  stmt' <- labelStatementLoops stmt
  _ <- popStack
  return $ ForStatement forInit expr forPost stmt' label


labelStatementLoops (ContinueStatement _) = do
  loopId <- findLastLoopId
  label <- case loopId of
    Just loopId' -> return $ makeLoopLabel loopId'
    Nothing -> do
      tell ["'continue' statement outside of a loop!"]
      return $ LoopLabel "ERROR"
  return $ ContinueStatement label


labelStatementLoops (BreakStatement _) = do
  stackEntry <- peekStack
  target <- case stackEntry of
    Just (IdStackLoopEntry loopId) -> return . BreakTargetLoop . makeLoopLabel $ loopId
    Just (IdStackSwitchEntry switchId) -> return . BreakTargetSwitch . makeSwitchLabel $ switchId
    Nothing -> do
      tell ["'break' statement outside of a loop or switch statement!"]
      return . BreakTargetLoop . LoopLabel $ "ERROR"
  return $ BreakStatement target


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

labelStatementLoops (SwitchStatement expr items _) = do
  label <- makeSwitchLabel <$> pushSwitch
  items' <- mapM labelSwitchItemLoops items
  _ <- popStack
  return $ SwitchStatement expr items' label


labelSwitchItemLoops :: SwitchItem -> Labeler SwitchItem
labelSwitchItemLoops (SwitchItemCase value _) = SwitchItemCase value <$> getSwitchCaseLabel (Just value)
labelSwitchItemLoops (SwitchItemDefaultCase _) = SwitchItemDefaultCase <$> getSwitchCaseLabel Nothing
labelSwitchItemLoops (SwitchItemStatement stmt) =
  SwitchItemStatement <$> labelStatementLoops stmt


makeLoopLabel :: LoopId -> LoopLabel
makeLoopLabel loopId = LoopLabel $ "loop" ++ show loopId

makeSwitchLabel :: SwitchId -> SwitchLabel
makeSwitchLabel switchId = SwitchLabel $ "switch" ++ show switchId


getSwitchCaseLabel :: Maybe SwitchCaseValue -> Labeler String
getSwitchCaseLabel value = do
  (counter, stack) <- get
  put (counter + 1, stack)
  let description = maybe "default" show value
  return $ "switchCase_" ++ description ++ "_" ++ show (counter + 1)


pushLoop :: Labeler LoopId
pushLoop = do
  (counter, stack) <- get
  let newLoopId = counter + 1
  put (newLoopId, IdStackLoopEntry newLoopId : stack)
  return newLoopId


pushSwitch :: Labeler SwitchId
pushSwitch = do
  (counter, stack) <- get
  let newSwitchId = counter + 1
  put (newSwitchId, IdStackSwitchEntry newSwitchId : stack)
  return newSwitchId


popStack :: Labeler (Maybe IdStackEntry)
popStack = do
  (counter, stack) <- get
  case stack of
    (entry : stack') -> do
      put (counter, stack')
      return $ Just entry
    [] -> return Nothing


-- A 'break' statement will break out of whatever we saw last.
peekStack :: Labeler (Maybe IdStackEntry)
peekStack = do
  (_, stack) <- get
  case stack of
    (entry : _) -> return $ Just entry
    [] -> return Nothing


-- A 'continue' statement can only work inside a loop.
findLastLoopId :: Labeler (Maybe LoopId)
findLastLoopId = do
  (_, stack) <- get
  return $ go stack
  where
    go :: IdStack -> Maybe LoopId
    go [] = Nothing
    go (IdStackLoopEntry loopId : _) = Just loopId
    go (_ : xs) = go xs


