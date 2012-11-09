
module AbstractSyntaxTree where

import Data.List (intersperse)

type Program = Statement

data Statement = Variable :=: Expression Int
               | While (Condition Int) Statement
               | Statement ::: Statement
               | Funct Variable Variable [Variable] Statement
               deriving (Eq)
infix 2 :=:
infixr 1 :::
infix 1 :>:

instance Show Statement where
    show (n :=: e)          = n ++ " = " ++ (show e)
    show (While c s)        = "while (" ++ (show c) ++ ") {" ++ (show s) ++ "}"
    show (s1 ::: s2)        = (show s1) ++ "; " ++ (show s2)
    show (Funct n r p s)    = "function " ++ n ++ " (" ++ (concat $ intersperse ", " p)
                              ++ ") -> " ++ r ++ " {" ++ (show s) ++ "}"

data Expression a = N a
                  | V Variable
                  | C Variable [Expression a]
                  | Expression a :+: Expression a
                  | Expression a :-: Expression a
                  | Expression a :*: Expression a
                  deriving (Eq)

instance Show a => Show (Expression a) where
    show (N c)          = show c
    show (V n)          = n
    show (C n es)       = n ++ "(" ++ (concat $ intersperse ", " $ map show es) ++ ")"
    show (e1 :+: e2)    = "(" ++ (show e1) ++ " + " ++ (show e2) ++ ")"
    show (e1 :-: e2)    = "(" ++ (show e1) ++ " - " ++ (show e2) ++ ")"
    show (e1 :*: e2)    = "(" ++ (show e1) ++  "*"  ++ (show e2) ++ ")"

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

instance Functor Expression where
    fmap f = foldExpression (N . f) (V) (C) (:+:) (:-:) (:*:)

data Condition a = Expression a :>: Expression a
                 deriving (Eq)

instance Show a => Show (Condition a) where
    show (e1 :>: e2) = (show e1) ++ " > " ++ (show e2)

type Variable = String

data Value = Const Int | Function ([Int] -> Int)

instance Show Value where
    show (Const c)      = show c
    show (Function _)   = "(function)"

instance Eq Value where
    (Const x) == (Const y)          = x == y
    (Function _) == (Function _)    = False
