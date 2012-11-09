
module PrettyPrinter where

import Prelude       hiding (filter)
import AbstractSyntaxTree
import AnnotatedSyntaxTree
import VariableMap   hiding (filter)
import Data.Map             (toList, (!), filter)
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
    in (str1 ++ padding m (length str1) ++ printVars m asm ++ "\n" ++ str2 ++ str3
       , max (length str1) m1
       )
printProgram' i m (AnnotatedFunction v r ps s) =
    let str1 = indent i ++ "function " ++ v ++ "(" ++
               (concat $ intersperse ", " ps) ++ ") -> " ++ r ++ " {"
        (str2, m1) = printProgram' (i + 1) m s
        str3 = indent i ++ "}"
    in (str1 ++ "\n" ++ str2 ++ "\n" ++ str3, max (length str1) m1)

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

printVars :: Indent -> [VariableMap Value] -> String
printVars i []      = ""
printVars i (m:ms)  = concat
                      $ intersperse
                        (replicate i ' ') 
                        (map (\(x, y) -> printVar x y ++ "\n")
                         $ foldl merge (map (\(x, y) -> (x, [y])) $ toList $ filterInt m) ms)
  where filterInt :: VariableMap Value -> VariableMap Int
        filterInt m = fmap (\(Const c) -> c) $ filter (isInt) m
          where isInt x = case x of Const c -> True
                                    _       -> False
        merge :: [(Variable, [Int])] -> VariableMap Value -> [(Variable, [Int])]
        merge ps m = map (\(v, is) -> (v, is ++ [filterInt m ! v])) ps
