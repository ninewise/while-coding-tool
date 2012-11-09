
module VariableMap where

import Prelude  hiding (filter)
import Data.Map hiding (filter)

import AbstractSyntaxTree

type VariableMap a = Map String a

empty :: VariableMap a
empty = Data.Map.empty

lookup :: VariableMap a -> Variable -> Maybe a
lookup m k = Data.Map.lookup k m

update :: VariableMap a -> Variable -> a -> VariableMap a
update m k v = Data.Map.insert k v m

filter :: VariableMap a -> [Variable] -> VariableMap a
filter m []     = m
filter m (v:vs) = filter (delete v m) vs

