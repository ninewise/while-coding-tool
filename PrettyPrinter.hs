
module PrettyPrinter where

import Prelude       hiding (filter)
import AbstractSyntaxTree
import AnnotatedSyntaxTree
import VariableMap   hiding (filter)
import Data.Map             (toList, (!), filter)
import Data.List            (intersperse)

-- . A module to transform an Annotated Program into a String, ready to be put
-- | to the output. It contains additonal functions for indenting, aligning and
-- the printing of a Variable and a List of Integers nicely.
-- `---------------------------------------------------------------------------

printProgram :: AnnotatedStatement -> String
printProgram s = let (str, m) = printProgram' 0 m s in str

-- Just a synonym, to clarify type declarations.
type Indent = Int

-- Recursive method to print an AnnotatedStatement. The first Indent indicates
-- the leading indent. The second is the maximal length of each printed state-
-- ment. This value if used for aligning the annotations. To ensure we need to
-- traverse the statements only once, we calcalute this lazily.
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

-- Get the whitespace needed to indent Int levels.
indent :: Int -> String
indent i | i < 1        = ""
         | otherwise    = "    " ++ indent (i - 1)

-- Get the whitespace needed to extend a string of length min to the pref (pre-
-- ferred) length. If the miimum length is longer than the preferred length, no
-- padding is used (creating overflow).
padding :: Int -> Int -> String
padding pref min | pref > min   = replicate (pref - min) ' '
                 | otherwise    = ""

-- Print the given variable and his sequence of values in the form
--  "x = 1 | 2 | 3"
printVar :: Variable -> [Int] -> String
printVar v []       = ""
printVar v [i]      = " :   " ++ v ++ " = " ++ show i
printVar v (i:is)   = printVar v is ++ " | " ++ show i

-- Print the variables in the list of variablemaps as the method above.
-- However, since this method shows different variables on different lines,
-- it also needs the indent of the original lines to align the others.
printVars :: Indent -> [VariableMap Value] -> String
printVars i []      = ""
printVars i (m:ms)  = concat $ intersperse -- join the list of strings with
                        (replicate i ' ')  -- indent whitespace.
                        -- print each variable with it's values.
                        (map (\(x, y) -> printVar x y ++ "\n")
                         -- convert the list of variablemaps to a tuple of
                         -- variables and a list of their values.
                         $ foldl merge (map (\(x, y) -> (x, [y])) $ toList $ filterInt m) ms)
  where -- filter the functions from the VariableMap.
        filterInt :: VariableMap Value -> VariableMap Int
        filterInt m = fmap (\(Const c) -> c) $ filter (isInt) m
          where isInt x = case x of Const c -> True
                                    _       -> False
        -- Append the values of the variables in the map to their respective
        -- variables in the list of tuples.
        merge :: [(Variable, [Int])] -> VariableMap Value -> [(Variable, [Int])]
        merge ps m = map (\(v, is) -> (v, is ++ [filterInt m ! v])) ps
