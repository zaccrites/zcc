{-# LANGUAGE BlockArguments #-}

module Main(main) where

import Control.Monad.Except
import System.Exit (ExitCode (..), exitWith)

import Control.Monad (forM_, when, foldM)
import Compiler.Lexer.Tokenizer
import Compiler.Lexer.SourceToken
import Compiler.Lexer.Token
import Compiler.Parser.Parser
import Compiler.Parser.ParserError
import Compiler.Parser.AstPrinting
import Compiler.CodeGen.Intermediate
import Compiler.CodeGen.AssemblyX64
import Compiler.SemanticAnalysis.VariableResolution
import Compiler.SemanticAnalysis.LoopAndSwitchLabeling


-- enumerate :: Integral b => [a] -> [(b, a)]
enumerate :: [a] -> [(Integer, a)]
enumerate = zip [1..]


-- TODO: Define a top-level "exit reason" enum type and map to ExitCode
main :: IO ()
main = run >>= exitWith

run :: IO ExitCode
run = do
  text <- readFile "/home/zac/code/zcc/sample.c"

  putStrLn "---------------------- Code ----------------------"
  let textLines = lines text
  forM_ (enumerate textLines) \(i, line) -> putStrLn (show i ++ ": " ++ line)
  putStrLn "\n"

  doLexProgram text


doLexProgram :: String -> IO ExitCode
doLexProgram text = do
  putStrLn "---------------------- Tokens ----------------------"
  let (tokens, lexerErrors) = getTextTokens text
  forM_ (enumerate tokens) \(i, token) -> putStrLn (show i ++ ": " ++ show token)
  when (null lexerErrors) do
    putStrLn ""
    putStrLn $ unwords $ map (\(SourceToken token _) -> tokenToString token) tokens
  putStrLn "\n"

  if null lexerErrors
    then doParseProgram tokens
    else do
      putStrLn "---------------------- Lexer Errors ----------------------"
      forM_ (enumerate lexerErrors ) \(i, err) -> putStrLn (show i ++ ": " ++ show err)
      putStrLn "\n"
      return $ ExitFailure 1


doParseProgram :: [SourceToken] -> IO ExitCode
doParseProgram tokens = do
  let (program, parserErrors, _) = parseProgram tokens
  case program of
    Nothing -> do
      putStrLn "Parser Failed!"
      if null parserErrors
        then putStrLn "  ... but there were no parser errors."
        else forM_ (enumerate parserErrors) \(i, err) -> putStrLn (show i ++ ": " ++ formatParserError err)
      return $ ExitFailure 1
    Just program' -> do
      putStrLn "---------------------- AST ----------------------"
      putStrLn . unlines . printAst $ program'
      putStrLn "\n"
      doSemanticAnalysis program'


doSemanticAnalysis :: Program -> IO ExitCode
doSemanticAnalysis program = do
  result <- runSteps
  case result of
    Left exitCode -> return exitCode
    Right program' -> doIrCodeGen program'

  where
    steps =
      [ doStep "Variable Resolution" resolveVariables
      , doStep "Label Loops and Switch Statements" labelLoopsAndSwitches
      ]

    runSteps = runExceptT $ foldM runStep program steps
      where
        runStep val step = ExceptT (step val)

    doStep :: Show e => String -> (Program -> (Program, [e])) -> Program -> IO (Either ExitCode Program)
    doStep name step stepProgram = do
      let (stepProgram', errors) = step stepProgram
      putStrLn $ "---------------------- " ++ name ++ " ----------------------"
      if null errors
        then forM_ (printAst stepProgram') putStrLn
        else forM_ (enumerate errors) \(i, err) -> putStrLn (show i ++ ": " ++ show err)
      putStrLn "\n"
      if null errors
          then return $ Right stepProgram'
          else return $ Left (ExitFailure 1)


doIrCodeGen :: Program -> IO ExitCode
doIrCodeGen program = do
  let irProgram = genIrProgram program

  putStrLn "---------------------- IR Program ----------------------"
  case irProgram of
    IrProgram (IrFuncDef _ instructions) ->
      forM_ (enumerate instructions) \(i, inst) -> putStrLn (show i ++ ": " ++ show inst)
  putStrLn "\n"

  doAsmCodeGen irProgram


doAsmCodeGen :: IrProgram -> IO ExitCode
doAsmCodeGen program = do
  let asmProgram = genAsmProgram program

  putStrLn "---------------------- ASM Program ----------------------"
  case asmProgram of
    AsmProgram (AsmFuncDef _ _ instructions) ->
      forM_ (enumerate instructions) \(i, inst) -> putStrLn (show i ++ ": " ++ show inst)
  putStrLn "\n"

  putStrLn "---------------------- ASM Text ----------------------"
  let asmText = showAsm asmProgram
  putStrLn asmText
  putStrLn "\n"

  writeFile "out.s" asmText
  return ExitSuccess

