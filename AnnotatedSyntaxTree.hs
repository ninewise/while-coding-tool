
module AnnotatedSyntaxTree where

import Prelude  hiding (filter, lookup)
import Data.Map (fromList, union, (!))

import AbstractSyntaxTree
import VariableMap
import Evaluator

-- . A module containing the Annotated Program Data structure, and a function
-- | to annotate a Statement.
-- |
-- | The AnnotatedStatement Datatype contains a corresponding constructor for
-- | each of the constructors of Statement. The difference is, that assignments
-- | gain a memory of the values of their assigned variable and that while does
-- | the same, but for all variables in the scope.
-- `---------------------------------------------------------------------------

type AnnotatedProgram = AnnotatedStatement

data AnnotatedStatement
    = AnnotatedAssignment Variable (Expression Int) [Int]
    | AnnotatedWhile (Condition Int) AnnotatedStatement [VariableMap Value]
    | AnnotatedSequence AnnotatedStatement AnnotatedStatement
    | AnnotatedFunction Variable Variable [Variable] AnnotatedStatement
    deriving (Show, Eq)

annotateProgram :: Program -> AnnotatedProgram
annotateProgram = fst . (\x -> fill x empty) . annotate

-- transform a Statement to an AnnotatedStatement, with empty memories. Then it
-- is ready to be executed.
annotate :: Statement -> AnnotatedStatement
annotate (v :=: e)          = AnnotatedAssignment v e []
annotate (x ::: y)          = AnnotatedSequence (annotate x) (annotate y)
annotate (While c s)        = AnnotatedWhile c (annotate s) []
annotate (Funct n r ps s)   = AnnotatedFunction n r ps (annotate s)

-- Fill the AnnotatedStatement using the variables in the map. Returns the
-- filled annotated statement and the new map resulting from the program.
fill :: AnnotatedStatement -> VariableMap Value -> (AnnotatedStatement, VariableMap Value)

fill (AnnotatedAssignment v e as) m =
    case calculate m e of
    Just a  -> (AnnotatedAssignment v e $ a:as, update m v $ Const a)
    Nothing -> error "Couldn't retrieve variable."

fill (AnnotatedSequence x y) m =
    let (as1, m1) = fill x m
        (as2, m2) = fill y m1
    in (AnnotatedSequence as1 as2, m2)

fill (AnnotatedWhile c s asm) m =
    case check m c of
    Just True   -> let (asm1, m1) = fill s m
                   in fill (AnnotatedWhile c asm1 $ m:asm) m1
    Just False  -> (AnnotatedWhile c s $ m:asm, m)
    Nothing -> error "Couldn't retrieve variable."

fill (AnnotatedFunction n r ps s) m =
    -- Insert the function in the VariableMap, using that updated map to create
    -- the function. This way, we can use the function inside itself, enabling
    -- recursion.
    let mx = update m n $ Function (\vs -> forceInt (
               snd -- the second argument is the map.
               $ fill s -- run the statement with the created map
               $ update -- insert the recursive function.
                -- combine the variables with their values.
                (fromList $ zipWith (\x y -> (x, Const y)) ps vs)
                -- the function name and it's value to be.
                n (mx ! n)
            ) r)
    in (AnnotatedFunction n r ps s, mx)
      where -- Looks up the variable in the map, but crashes if it does not
            -- point to in Integer.
            forceInt :: VariableMap Value -> Variable -> Int
            forceInt m v = case lookup m v of
                Just (Const c)  -> c
                _               -> error $ "Forcing an Int from " ++ v ++ " failed in " ++ show m ++ "."

