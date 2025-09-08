{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main(main) where

import Control.Monad (forM_, unless, when)
import Compiler.Lexer.Tokenizer
import Compiler.Lexer.SourceToken
import Compiler.Lexer.Token
import Compiler.Parser.Parser
import Compiler.Parser.ParserError
import Compiler.CodeGen.Intermediate
import Compiler.CodeGen.AssemblyX64

-- enumerate :: Integral b => [a] -> [(b, a)]
enumerate :: [a] -> [(Integer, a)]
enumerate = zip [1..]


main :: IO ()
main = do
  text <- readFile "/home/zac/code/zcc/sample.c"
  -- putStr text

  putStrLn "---------------------- Code ----------------------"
  let textLines = lines text
  forM_ (enumerate textLines) \(i, line) -> putStrLn (show i ++ ": " ++ line)
  putStrLn "\n"

  doLexProgram text



doLexProgram :: String -> IO ()
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


doParseProgram :: [SourceToken] -> IO ()
doParseProgram tokens = do
  let (program, parserErrors, tokens') = parseProgram tokens
  case program of
    Nothing -> do
      putStrLn "Parser Failed!"
      if null parserErrors
        then putStrLn "  ... but there were no parser errors."
        else forM_ (enumerate parserErrors) \(i, err) -> putStrLn (show i ++ ": " ++ formatParserError err)
    Just program' -> do
      putStrLn "---------------------- AST ----------------------"
      print program'
      putStrLn "\n"
      doIrCodeGen program'


doIrCodeGen :: Program -> IO ()
doIrCodeGen program = do
  let irProgram = genIrProgram program

  putStrLn "---------------------- IR Program ----------------------"
  case irProgram of
    IrProgram (IrFuncDef _ instructions) ->
      forM_ (enumerate instructions) \(i, inst) -> putStrLn (show i ++ ": " ++ show inst)
    _ -> print irProgram
  putStrLn "\n"

  doAsmCodeGen irProgram
  --

doAsmCodeGen :: IrProgram -> IO ()
doAsmCodeGen program = do
  let asmProgram = genAsmProgram program

  putStrLn "---------------------- ASM Program ----------------------"
  case asmProgram of
    AsmProgram (AsmFuncDef _ _ instructions) ->
      forM_ (enumerate instructions) \(i, inst) -> putStrLn (show i ++ ": " ++ show inst)
    _ -> print asmProgram
  putStrLn "\n"

  putStrLn "---------------------- ASM Text ----------------------"
  let asmText = showAsm asmProgram
  putStrLn asmText
  putStrLn "\n"

  writeFile "out.s" asmText

