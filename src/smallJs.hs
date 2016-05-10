{-
  Name: Wing Cheong Tam
  Class: CS 252
  Assigment: Javascript Interpreter
  Date: 4-3-2015
  Description: Parser and Interpreter for Javascript
-}
import System.IO
import System.Console.GetOpt
import System.Environment(getArgs, getProgName)
import Data.Map (Map)
import qualified Data.Map as Map

import Ast
import Parser
import Interp

showParsedExp :: String -> IO ()
showParsedExp fileName = do
  p <- parseFromFile fileP fileName
  case p of
    Left parseErr -> print parseErr
    Right exp -> print exp

runFile :: String -> IO ()
runFile fileName = do
  p <- parseFromFile fileP fileName
  case p of
    Left parseErr -> print parseErr
    Right exp ->
      --putStrLn $ show exp
      case (exec exp) of
        Left msg -> print msg
        Right (v, h:hs) -> printScreen h

printScreen :: Map String Value -> IO ()
printScreen h = do
  putStrLn $ "\nScreen (Output Stream):"
  putStrLn $ "######################################################################"
  putStrLn $ (case Map.lookup _ostream_mem_loc h of
    Just (StrVal output)  -> output
    _ -> "Memory location for screen output is corrupted")
  putStrLn $ "\nHeap (Memory):"
  putStrLn $ "######################################################################"
  putStrLn $ printHeap $ Map.toList h
  putStrLn $ (case Map.lookup _ref_mem_loc h of
    Just (RefVal output)  -> "mem_pointer : " ++ (printHeapValue (RefVal output))
    _ -> "Memory location for screen output is corrupted")

printHeap :: [(String, Value)] -> String
printHeap [] = ""
printHeap ((k,v):xs) =
  if k == _ostream_mem_loc then 
    printHeap xs
  else if k == _ref_mem_loc then
    printHeap xs
  else if isInteger k then
    pad (printHeapValue (RefVal (read k))) ++ ": " ++ printHeapValue v ++ "\n" ++ printHeap xs
  else case v of
    StrVal str -> pad k ++ ": \"" ++ str ++ "\"\n" ++ printHeap xs
    _ -> pad k ++ ": " ++ printHeapValue v ++ "\n" ++ printHeap xs

pad :: String -> String
pad k =
  if length k >= (_heap_disply_pad :: Int)
    then k
    else pad (k ++ _disply_pad_char:[])

launchCmdPrompt history = do
  putStr "> "
  hFlush stdout
  cmd <- getLine
  case cmd of
    "quit"    -> putStrLn "Leaving smallJs."
    "memory"  -> case history of
      Just (h:hs) -> do {printMemory h; launchCmdPrompt history}
      Nothing -> do {putStrLn "<empty>"; launchCmdPrompt history}
    _         -> case execJsBlk cmd of
      Left msg -> print msg
      Right exp -> case history of
        Nothing -> case (exec exp) of
          Left msg -> do {print msg; launchCmdPrompt history}
          Right (v, heap@(h:hs)) -> do {printCmdScreen h; launchCmdPrompt (Just heap)}
        Just history' -> case (resume exp history') of
          Left msg -> do {print msg; launchCmdPrompt history}
          Right (v, heap@(h:hs)) -> do {printCmdScreen h; launchCmdPrompt (Just heap)}

printCmdScreen h = do
  putStr $ (case Map.lookup _ostream_mem_loc h of
    Just (StrVal output)  -> output
    _ -> "Memory location for screen output is corrupted")
  hFlush stdout

printMemory h = do
  putStrLn $ "#################  MEMORY  ###############"
  putStr   $ printHeap $ Map.toList h
  hFlush stdout
  putStrLn $ (case Map.lookup _ref_mem_loc h of
    Just (RefVal output)  -> "[MEMORY_POINTER " ++ (printHeapValue (RefVal output)) ++ "]"
    _ -> "Memory location for screen output is corrupted")
  putStrLn $ "##########################################"

main :: IO ()
main = do
  args <- getArgs
  if args == [] || (head args) == "-h" || (head args) == "--help"
  	then do {
      putStrLn "usage: smallJs [options] script.js\n";
      putStrLn "Options:";
      putStrLn "  -i, --interactive         enter interactive mode";
      putStrLn "  -h, --help                list user manual\n"
    }
  	else do {
  		fileName <- return $ head args;
      if fileName == "-i" || (head args) == "--interactive"
        then do
          putStrLn "\n---- smallJs interactive mode ----\n"
          putStrLn "Commands:"
          putStrLn "quit          Terminate the program"
          putStrLn "memory        Show the memory state\n"
          launchCmdPrompt Nothing
        else do
      		putStrLn $ "executing " ++ fileName ++ "..."
      		runFile fileName
    }
  


