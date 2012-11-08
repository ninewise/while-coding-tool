
module VariableMap where

import Data.Map

import AbstractSyntaxTree

type VariableMap a = Map String a

empty :: VariableMap a
empty = Data.Map.empty

lookup :: VariableMap a -> Variable -> Maybe a
lookup m k = Data.Map.lookup k m

update :: VariableMap a -> Variable -> a -> VariableMap a
update m k v = Data.Map.insert k v m

