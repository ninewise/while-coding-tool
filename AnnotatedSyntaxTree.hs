
module AnnotatedSyntaxTree where

import Prelude  hiding (filter, lookup)
import Data.Map (fromList, union)

import AbstractSyntaxTree
import VariableMap
import Evaluator

type AnnotatedProgram = AnnotatedStatement

data AnnotatedStatement
    = AnnotatedAssignment Variable (Expression Int) [Int]
    | AnnotatedWhile (Condition Int) AnnotatedStatement [VariableMap Value]
    | AnnotatedSequence AnnotatedStatement AnnotatedStatement
    | AnnotatedFunction Variable Variable [Variable] AnnotatedStatement
    deriving (Show, Eq)

annotateProgram :: Statement -> AnnotatedStatement
annotateProgram = fst . (\x -> fill x empty) . annotate

annotate :: Statement -> AnnotatedStatement
annotate (v :=: e)          = AnnotatedAssignment v e []
annotate (x ::: y)          = AnnotatedSequence (annotate x) (annotate y)
annotate (While c s)        = AnnotatedWhile c (annotate s) []
annotate (Funct n r ps s)   = AnnotatedFunction n r ps (annotate s)

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
    let mx = update m n $ Function (\vs -> forceInt (snd $ fill s $ union (fromList $ zipWith (\x y -> (x, Const y)) ps vs) (filter mx ps)) r)
    in (AnnotatedFunction n r ps s, mx)
      where forceInt :: VariableMap Value -> Variable -> Int
            forceInt m v = case lookup m v of
                Just (Const c)  -> c
                _               -> error $ "Forcing an Int from " ++ v ++ " failed in " ++ show m ++ "."

