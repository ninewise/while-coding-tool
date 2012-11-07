
module Evaluator where

import Prelude hiding (lookup)
import AbstractSyntaxTree
import VariableMap
import Control.Monad

calculate :: VariableMap Int -> Expression Int -> Maybe Int
calculate m = foldExpression (Just) (lookup m) (liftM2 (+)) (liftM2 (-)) (liftM2 (*))

check :: VariableMap Int -> Condition Int -> Maybe Bool
check m (e1 :>: e2) = liftM2 (>) (calculate m e1) (calculate m e2)

evaluate :: VariableMap Int -> Statement -> VariableMap Int
evaluate m (v :=: e)    = case calculate m e of
                            Just c  -> update m v c
                            Nothing -> error "Unable to calculate expression."
evaluate m (s1 ::: s2)  = evaluate (evaluate m s1) s2
evaluate m (While c s)  = case check m c of
                            Just True   -> evaluate (evaluate m s) (While c  s)
                            Just False  -> m
                            Nothing     -> error "Unable to check the condition."
