
module Evaluator where

import Prelude hiding (lookup)
import AbstractSyntaxTree
import VariableMap
import Control.Monad

-- . A small module containing the logic to calculate expressions and check
-- | conditions.
-- `---------------------------------------------------------------------------

-- calculate the Expression with the variables and values in the map.
calculate :: VariableMap Value -> Expression Int -> Maybe Int
calculate m = foldExpression (Just) (retrieve) (call) (liftM2 (+)) (liftM2 (-)) (liftM2 (*))
  where -- Retrieve a integer variable from the map. Crashes when the
        -- variable's actually no integer, but a function.
        retrieve :: Variable -> Maybe Int
        retrieve s = case lookup m s of
            Just (Const c)  -> return c
            Nothing         -> Nothing
        -- Call the function pointed to by the Variable with the given
        -- arguments. If the variable doesn't point to a function, happily
        -- crashes.
        call :: Variable -> [Maybe Int] -> Maybe Int
        call s ps = case lookup m s of
            Just (Function f)   -> sequence ps >>= return . f
            Nothing             -> Nothing

check :: VariableMap Value -> Condition Int -> Maybe Bool
check m (e1 :>: e2) = liftM2 (>) (calculate m e1) (calculate m e2)
