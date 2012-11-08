
module AnnotatedSyntaxTree where

import AbstractSyntaxTree
import VariableMap
import Evaluator

type AnnotatedProgram = AnnotatedStatement

data AnnotatedStatement
    = AnnotatedAssignment Variable (Expression Int) [Int]
    | AnnotatedWhile (Condition Int) AnnotatedStatement [VariableMap Int]
    | AnnotatedSequence AnnotatedStatement AnnotatedStatement
    deriving (Show, Eq)

annotateProgram :: Statement -> AnnotatedStatement
annotateProgram = fst . (\x -> fill x empty) . annotate

annotate :: Statement -> AnnotatedStatement
annotate (v :=: e)      = AnnotatedAssignment v e []
annotate (x ::: y)      = AnnotatedSequence (annotate x) (annotate y)
annotate (While c s)    = AnnotatedWhile c (annotate s) []

fill :: AnnotatedStatement -> VariableMap Int -> (AnnotatedStatement, VariableMap Int)

fill (AnnotatedAssignment v e as) m =
    case calculate m e of
    Just a  -> (AnnotatedAssignment v e $ a:as, update m v a)
    Nothing -> (AnnotatedAssignment v e as, m)

fill (AnnotatedSequence x y) m =
    let (as1, m1) = fill x m
        (as2, m2) = fill y m1
    in (AnnotatedSequence as1 as2, m2)

fill (AnnotatedWhile c s asm) m =
    case check m c of
    Just True   -> let (asm1, m1) = fill s m
                   in fill (AnnotatedWhile c asm1 $ m:asm) m1
    Just False  -> (AnnotatedWhile c s $ m:asm, m)
    Nothing     -> (AnnotatedWhile c s asm, m)

