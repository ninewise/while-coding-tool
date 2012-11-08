
module Main where

import System.Environment

import WhileParser          (parseProgram)
import AnnotatedSyntaxTree  (annotateProgram)
import PrettyPrinter        (printProgram)

main :: IO ()
main = do file:_ <- getArgs
          contents <- readFile file
          putStr $ printProgram $ annotateProgram $ parseProgram contents

