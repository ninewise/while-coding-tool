
module VariableMap where

import Prelude  hiding (filter)
import Data.Map hiding (filter)

import AbstractSyntaxTree

-- . A `VariableMap a` is a wrapper around a `Map Variable a`. As such, I can
-- | choose the names and signature of functions without relying on the
-- | interface of the Data.Map. I can also extend it with my own filter
-- | function.
-- `---------------------------------------------------------------------------

type VariableMap a = Map String a

-- An empty map.
empty :: VariableMap a
empty = Data.Map.empty

-- Might retrieve the given variable from the map.
lookup :: VariableMap a -> Variable -> Maybe a
lookup m k = Data.Map.lookup k m

-- If Variable is already defined in the map, update it's value. Otherwise,
-- insert the Variable.
update :: VariableMap a -> Variable -> a -> VariableMap a
update m k v = Data.Map.insert k v m

-- Remove all variables form the map, except the ones given. This is useful for
-- filtering the variable scope within functions.
filter :: VariableMap a -> [Variable] -> VariableMap a
filter m []     = m
filter m (v:vs) = filter (delete v m) vs

