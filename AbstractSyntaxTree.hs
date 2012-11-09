
module AbstractSyntaxTree where

import Data.List (intersperse)

-- . The module containing the base Data Structure of the whole program: The
-- | Abstract Syntax Tree. It's defined recursive, just as the syntax
-- | definition in the assignment.
-- `---------------------------------------------------------------------------

type Program = Statement

data Statement = Variable :=: Expression Int
               | While (Condition Int) Statement
               | Statement ::: Statement
               | Funct Variable Variable [Variable] Statement
               deriving (Eq)

-- To spare me quite a bit typing of parentheses.
infix 2 :=:
infixr 1 :::
infix 1 :>:

-- Not really used, but good for your eyes when debugging.
instance Show Statement where
    show (n :=: e)          = n ++ " = " ++ (show e)
    show (While c s)        = "while (" ++ (show c) ++ ") {" ++ (show s) ++ "}"
    show (s1 ::: s2)        = (show s1) ++ "; " ++ (show s2)
    show (Funct n r p s)    = "function " ++ n ++ " (" ++ (concat $ intersperse ", " p)
                              ++ ") -> " ++ r ++ " {" ++ (show s) ++ "}"

-- The expression data type. Just recursive defined.
data Expression a = N a
                  | V Variable
                  | C Variable [Expression a]
                  | Expression a :+: Expression a
                  | Expression a :-: Expression a
                  | Expression a :*: Expression a
                  deriving (Eq)

-- How to show expressions.
instance Show a => Show (Expression a) where
    show (N c)          = show c
    show (V n)          = n
    show (C n es)       = n ++ "(" ++ (concat $ intersperse ", " $ map show es) ++ ")"
    show (e1 :+: e2)    = "(" ++ (show e1) ++ " + " ++ (show e2) ++ ")"
    show (e1 :-: e2)    = "(" ++ (show e1) ++ " - " ++ (show e2) ++ ")"
    show (e1 :*: e2)    = "(" ++ (show e1) ++  "*"  ++ (show e2) ++ ")"

-- Only used to calculate expressions and define a Functor instance.
foldExpression :: (a -> t) -> (Variable -> t) -> (String -> [t] -> t)
               -> (t -> t -> t) -> (t -> t -> t) -> (t -> t -> t)
               -> Expression a -> t
foldExpression con var fun add sub mul = go
  where go (N c)        = con c
        go (V v)        = var v
        go (C n es)     = fun n (map go es)
        go (x :+: y)    = add (go x) (go y)
        go (x :-: y)    = sub (go x) (go y)
        go (x :*: y)    = mul (go x) (go y)

-- defining a Functor instance. Because I can. It's not used anywhere.
instance Functor Expression where
    fmap f = foldExpression (N . f) (V) (C) (:+:) (:-:) (:*:)

-- Similar to an expression, except it returns a bool when evaluated, not
-- an a.
data Condition a = Expression a :>: Expression a
                 deriving (Eq)

instance Show a => Show (Condition a) where
    show (e1 :>: e2) = (show e1) ++ " > " ++ (show e2)

-- Synonym to clarify type declarations.
type Variable = String

-- . Variable support
-- | By changing the Value datatype, it's quite easy to add more types for the
-- | variables. I added functions, but it would be hardly any work to add
-- | floats (by changing Int to Num a) or other types.
-- `---------------------------------------------------------------------------
data Value = Const Int | Function ([Int] -> Int)

-- Because functions cannot be shown easily.
instance Show Value where
    show (Const c)      = show c
    show (Function _)   = "(function)"

-- Because functions cannot be compared easily.
instance Eq Value where
    (Const x) == (Const y)          = x == y
    (Function _) == (Function _)    = False
