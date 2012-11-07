
module Main where

import System.Environment

import WhileParser
import VariableMap
import ParserM
import Evaluator

main :: IO ()
main = do file:_ <- getArgs
          contents <- readFile file
          let map = runProgram contents
          printVars map

runProgram :: String -> VariableMap Int
runProgram s = case runP program s of
                    Just x  -> evaluate empty x
                    Nothing -> error "Failed to parse program."
