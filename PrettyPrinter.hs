
module PrettyPrinter where

import AbstractSyntaxTree
import AnnotatedSyntaxTree
import VariableMap
import Data.Map             (toList, (!))
import Data.List            (intersperse)

printProgram :: AnnotatedStatement -> String
printProgram s = let (str, m) = printProgram' 0 m s in str

type Indent = Int
printProgram' :: Indent -> Indent -> AnnotatedStatement -> (String, Indent)
printProgram' i m (AnnotatedAssignment v e as) =
    let s = indent i ++ show (v :=: e)
    in (s ++ padding m (length s) ++ printVar v as, length s)
printProgram' i m (AnnotatedSequence s1 s2) =
    let (str1, m1) = printProgram' i m s1
        (str2, m2) = printProgram' i m s2
    in (str1 ++ "\n" ++ str2 ++ "\n", max m1 m2)
printProgram' i m (AnnotatedWhile c s asm) =
    let str1 = indent i ++ "while (" ++ show c ++ ") {"
        (str2, m1) = printProgram' (i + 1) m s
        str3 = indent i ++ "}"
    in (str1 ++ padding m (length str1) ++ printVars m asm ++ str2 ++ str3
       , max (length str1) m1
       )

indent :: Int -> String
indent i | i < 1        = ""
         | otherwise    = "    " ++ indent (i - 1)

padding :: Int -> Int -> String
padding pref min | pref > min   = replicate (pref - min) ' '
                 | otherwise    = ""

printVar :: Variable -> [Int] -> String
printVar v []       = ""
printVar v [i]      = " :   " ++ v ++ " = " ++ show i
printVar v (i:is)   = printVar v is ++ " | " ++ show i

printVars :: Int -> [VariableMap Int] -> String
printVars i []      = ""
printVars i (m:ms)  = concat
                      $ intersperse
                        (replicate i ' ') 
                        (map (\(x, y) -> printVar x y ++ "\n")
                         $ foldl merge (map (\(x, y) -> (x, [y])) $ toList m) ms)
  where merge :: [(Variable, [Int])] -> VariableMap Int -> [(Variable, [Int])]
        merge ps m = map (\(v, is) -> (v, is ++ [m ! v])) ps
