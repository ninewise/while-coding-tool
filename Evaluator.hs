
module Evaluator where

import Prelude hiding (lookup)
import AbstractSyntaxTree
import VariableMap
import Control.Monad

calculate :: VariableMap Int -> Expression Int -> Maybe Int
calculate m = foldExpression (Just) (lookup m) (liftM2 (+)) (liftM2 (-)) (liftM2 (*))

check :: VariableMap Int -> Condition Int -> Maybe Bool
check m (e1 :>: e2) = liftM2 (>) (calculate m e1) (calculate m e2)

