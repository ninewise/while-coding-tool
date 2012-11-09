
module Evaluator where

import Prelude hiding (lookup)
import AbstractSyntaxTree
import VariableMap
import Control.Monad

calculate :: VariableMap Value -> Expression Int -> Maybe Int
calculate m = foldExpression (Just) (retrieve) (call) (liftM2 (+)) (liftM2 (-)) (liftM2 (*))
  where retrieve :: Variable -> Maybe Int
        retrieve s = case lookup m s of
            Just (Const c)  -> return c
            Nothing         -> Nothing
        call :: Variable -> [Maybe Int] -> Maybe Int
        call s ps = case lookup m s of
            Just (Function f)   -> sequence ps >>= return . f
            Nothing             -> Nothing

check :: VariableMap Value -> Condition Int -> Maybe Bool
check m (e1 :>: e2) = liftM2 (>) (calculate m e1) (calculate m e2)
