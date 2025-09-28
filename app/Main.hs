{-# LANGUAGE BlockArguments #-}

module Main(main) where

import Control.Monad.Except
import System.Exit (ExitCode (..), exitWith)
import Control.Monad (forM_, when, foldM)
import Data.Traversable (for)

import Compiler.Lexer.Tokenizer
import Compiler.Lexer.SourceToken
import Compiler.Lexer.Token
import Compiler.Parser.Parser
import Compiler.Parser.ParserError
import Compiler.Parser.AstPrinting
import Compiler.CodeGen.Intermediate
import Compiler.CodeGen.AssemblyX64.Base
import Compiler.CodeGen.AssemblyX64.ReplacePseudoRegisters
import Compiler.SemanticAnalysis.IdentifierResolution
import Compiler.SemanticAnalysis.TypeChecking
import Compiler.SemanticAnalysis.LoopAndSwitchLabeling
import Compiler.CodeGen.AssemblyX64.FixFunctionStackSizes


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
      [ doStep "Identifier Resolution" resolveIdentifiers
      , doStep "Type Checking" checkTypes
      , doStep "Label Loops and Switch Statements" labelLoopsAndSwitches
      ]

    runSteps = runExceptT $ foldM runStep program steps
      where
        runStep val step = ExceptT (step val)

    doStep = doCompileStep printAst



doIrCodeGen :: Program -> IO ExitCode
doIrCodeGen program = do
  let irProgram = genIrProgram program

  putStrLn "---------------------- IR Program ----------------------"
  case irProgram of
    IrProgram defs ->
      forM_ defs \(IrFunctionDefinition name params instructions) ->
        forM_ (enumerate instructions) \(i, inst) -> putStrLn (name ++ "[" ++ show i ++ "]: " ++ show inst)
  putStrLn "\n"

  doAsmCodeGen irProgram


doAsmCodeGen :: IrProgram -> IO ExitCode
doAsmCodeGen program = do

  putStrLn "---------------------- ASM Program ----------------------"
  let asmProgram = genAsmProgram program
  putStrLn . unlines . printAsmProgram $ asmProgram
  putStrLn "\n"

  putStrLn "---------------------- Replace Pseudo Registers ----------------------"
  let asmProgram' = replaceProgramPseudoRegisters asmProgram
  putStrLn . unlines . printAsmProgram $ asmProgram'
  putStrLn "\n"

  putStrLn "---------------------- Fix Function Stack Sizes ----------------------"
  let asmProgram'' = fixFunctionStackSizes asmProgram'
  putStrLn . unlines . printAsmProgram $ asmProgram''
  putStrLn "\n"

  putStrLn "---------------------- ASM Text ----------------------"
  let asmText = showAsm asmProgram''
  putStrLn asmText
  putStrLn "\n"

  writeFile "out.s" asmText
  return ExitSuccess

  -- where
  --   steps =
  --     [ doStep "Replace Pseudo Registers" replacePseudoRegisters
  --     ]
  --
  --   doStep :: Show e => String -> (AsmProgram -> (AsmProgram, [e])) -> AsmProgram -> IO (Either ExitCode Program)
  --   -- doStep name step stepProgram = do
  --   doStep = undefined



printIrProgram :: IrProgram -> [String]
printIrProgram (IrProgram defs) = concatMap f defs
  where
    f :: IrDefinition -> [String]
    f (IrFunctionDefinition name params instructions) =
      for (enumerate instructions) \(i, inst) ->
        name ++ "[" ++ show i ++ "]: " ++ show inst


printAsmProgram :: AsmProgram -> [String]
printAsmProgram (AsmProgram defs) = concatMap asmDefToString defs
  where
    asmDefToString :: AsmDefinition -> [String]
    asmDefToString (AsmFuncDef name _ instructions) =
      let f (i, inst) = name ++ "[" ++ show i ++ "]: " ++ show inst
      in map f (enumerate instructions)


doCompileStep :: Show e => (a -> [String]) -> String -> (a -> (a, [e])) -> a -> IO (Either ExitCode a)
doCompileStep printer name step stepValue = do
  let (stepValue', errors) = step stepValue
  putStrLn $ "---------------------- " ++ name ++ " ----------------------"
  if null errors
    then forM_ (printer stepValue') putStrLn
    else forM_ (enumerate errors) \(i, err) -> putStrLn (show i ++ ": " ++ show err)
  putStrLn "\n"
  if null errors
      then return $ Right stepValue'
      else return $ Left (ExitFailure 1)

