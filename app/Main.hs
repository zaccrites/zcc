{-# LANGUAGE BlockArguments #-}

module Main(main) where

import Control.Monad (forM_, unless, when)
import Compiler.Lexer.Tokenizer
import Compiler.Lexer.SourceToken
import Compiler.Lexer.Token
import Compiler.Parser.Parser
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
  putStrLn "---------------------- Tokens ----------------------"
  let (program, parserErrors, tokens') = parseProgram tokens
  case program of
    Nothing -> do
      forM_ (enumerate parserErrors) \(i, err) -> putStrLn (show i ++ ": " ++ show err)
    Just program' -> do
      print program'
      putStrLn "\n"
      doCodeGen program'


doCodeGen :: Program -> IO ()
doCodeGen program = do
  let asmProgram = genProgramAsm program
  let asmText = showAsm asmProgram
  putStrLn asmText
  writeFile "out.s" asmText


