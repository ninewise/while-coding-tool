
module AbstractSyntaxTree where

import Data.List (intersperse)

type Program = Statement

data Statement = Variable :=: Expression Int
               | While (Condition Int) Statement
               | Statement ::: Statement
               deriving (Eq)
infix 2 :=:
infixr 1 :::
infix 1 :>:

instance Show Statement where
    show (n :=: e)      = n ++ " = " ++ (show e)
    show (While c s)    = "while (" ++ (show c) ++ ") {" ++ (show s) ++ "}"
    show (s1 ::: s2)    = (show s1) ++ "; " ++ (show s2)

data Expression a = N a
                  | V Variable
                  | Expression a :+: Expression a
                  | Expression a :-: Expression a
                  | Expression a :*: Expression a
                  deriving (Eq)

instance Show a => Show (Expression a) where
    show (N c)          = show c
    show (V n)          = n
    show (e1 :+: e2)    = "(" ++ (show e1) ++ " + " ++ (show e2) ++ ")"
    show (e1 :-: e2)    = "(" ++ (show e1) ++ " - " ++ (show e2) ++ ")"
    show (e1 :*: e2)    = "(" ++ (show e1) ++  "*"  ++ (show e2) ++ ")"

foldExpression :: (a -> t) -> (Variable -> t) -> (t -> t -> t) ->
                  (t -> t -> t) -> (t -> t -> t) -> Expression a -> t
foldExpression con var add sub mul = go
  where go (N c)        = con c
        go (V v)        = var v
        go (x :+: y)    = add (go x) (go y)
        go (x :-: y)    = sub (go x) (go y)
        go (x :*: y)    = mul (go x) (go y)

instance Functor Expression where
    fmap f = foldExpression (N . f) (V) (:+:) (:-:) (:*:)

data Condition a = Expression a :>: Expression a
                 deriving (Eq)

instance Show a => Show (Condition a) where
    show (e1 :>: e2) = (show e1) ++ " > " ++ (show e2)

type Variable = String
