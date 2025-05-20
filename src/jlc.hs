
import System.Environment (getArgs)
import System.Exit
import System.Process
import System.IO
import System.FilePath.Posix

import LexJavalette
import ParJavalette
import ErrM

import TypeChecker
import Compiler
import Utils

-- Flag state datatype
data FlagState = FlagState { input :: String
                           , optimize :: Bool
                           }

-- Default flag state
initFlagState :: FlagState
initFlagState = FlagState { input = ""
                          , optimize = False
                          }

--------------------------------------------------------------------------------
-- Main program functions
--------------------------------------------------------------------------------

main :: IO ()
main = do args <- getArgs
          case parseFlags initFlagState args of
              Right flags -> do
                  let file = input flags
                      genFile = replaceExtension file ".ll"
                      asFile = replaceExtension file ".bc"
                  code <- readFile file
                  prog <- driver code
                  writeFile genFile prog
                  if optimize flags
                      then optAssemble genFile asFile
                      else assemble genFile asFile
                  link asFile
                  createExecutableScript
                  hPutStrLn stderr "OK"
              Left error -> do putStrLn error
                               putStrLn "Usage: jlc [-O] <SourceFile>"
                               exitFailure

-- Driver for running the parser, type checker and compiler
driver :: String -> IO String
driver s = case pProgram (myLexer s) of
               Bad err -> do putStrLn "SYNTAX ERROR"
                             putStrLn err
                             hPutStrLn stderr "ERROR"
                             exitFailure 
               Ok tree -> do res <- typecheck tree
                             case res of
                                 Left err -> do putStrLn "TYPE ERROR"
                                                putStrLn err
                                                hPutStrLn stderr "ERROR"
                                                exitFailure 
                                 Right tree -> compile tree

-- Parse the flags and either return a new state or an error message
parseFlags :: FlagState -> [String] -> Either String FlagState
parseFlags flags ("-O" : rest) =
    parseFlags (flags {optimize = True}) rest
parseFlags flags (file : rest) =
    case input flags of
        "" -> parseFlags (flags {input = file}) rest
        _ -> Left "Error: Multiple inputs"
parseFlags flags [] =
    case input flags of
        "" -> Left "Error: Missing input"
        _ -> Right flags

--------------------------------------------------------------------------------
-- External execution functions
--------------------------------------------------------------------------------

-- Run a command and wait for it to terminate, if it reports exit failure,
-- terminate this program and report exit failure
runCommandAndWait :: String -> IO ()
runCommandAndWait cmd = do p <- runCommand cmd
                           exit <- waitForProcess p
                           case exit of
                               ExitFailure _ -> do hPutStrLn stderr "ERROR"
                                                   exitFailure
                               ExitSuccess -> return ()

-- Assembles a .ll file
assemble :: String -> String -> IO ()
assemble infile outfile = runCommandAndWait $
    "llvm-as" +-+ infile +-+ "-o" +-+ outfile

-- Assembles and optimizes a .ll file
optAssemble :: String -> String -> IO ()
optAssemble infile outfile = runCommandAndWait $
    "cat" +-+ infile +-+ "| llvm-as | opt -std-compile-opts -o" +-+ outfile

-- Link a binary file with the runtime library and save it as "a.out.bc"
link :: String -> IO ()
link file = runCommandAndWait $ "llvm-link -f -o a.out.bc lib/runtime.bc" +-+ file

-- Script for executing "a.out.bc"
executableScript :: String
executableScript = unlines [ "#!/bin/sh"
                           , "lli a.out.bc ${1+\"$@\"}"
                           , ""
                           ]

-- Creates the script as "a.out" and makes it executable
createExecutableScript :: IO ()
createExecutableScript = do writeFile "a.out" executableScript
                            runCommandAndWait "chmod u+x a.out"
