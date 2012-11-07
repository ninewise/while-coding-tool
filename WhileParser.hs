
module WhileParser where

import Prelude hiding (sequence)
import AbstractSyntaxTree
import ParserM

-- . Additional functions.
-- `---------------------------------------------------------------------------

whitespace :: ParserM p => p ()
whitespace = return ()
          \/ do (char ' ' \/ char '\n' \/ char '\t')
                whitespace
                return ()

token :: ParserM p => String -> p String
token s = do whitespace
             x <- token' s
             whitespace
             return x
    where 
        token' :: ParserM p => String -> p String
        token' []       = return []
        token' (c:cs)   = do x <- char c
                             xs <- token' cs
                             return (x:xs)

-- . Parse the program.
-- |
-- | This is just a wrapper, as a program is a statement in the abstract syntax
-- | tree. In real life, however, a program can still be surrounded by extra
-- | whitespace.
-- `---------------------------------------------------------------------------

program :: ParserM p => p Program
program = do whitespace
             x <- statement
             whitespace
             return x

-- . Parse the statement.
-- `---------------------------------------------------------------------------

statement :: ParserM p => p Statement
statement = sequence \/ assignment \/ loop \/ (comment >>= \_ -> statement)

assignment :: ParserM p => p Statement
assignment = do v <- variable
                token "="
                e <- expression
                return $ v :=: e

loop :: ParserM p => p Statement
loop = do token "while("
          c <- condition
          token ")"
          token "{"
          s <- statement
          token "}"
          return $ While c s

sequence :: ParserM p => p Statement
sequence = do x <- (assignment \/ loop)
              token ";"
              y <- statement
              return $ x ::: y

comment :: ParserM p => p ()
comment = do token "/*"
             text
             whitespace
             return ()
              where text :: ParserM p => p ()
                    text = item >>= text'
                    text' x = do y <- item
                                 if x == '*' && y == '/' then return ()
                                                         else text' y
             

-- . Parse an expression.
-- `---------------------------------------------------------------------------

-- expression and expression' are a left-associative addition and substraction
-- parser.
expression :: ParserM p => p (Expression Int)
expression = do e <- expr [mul, par, var, con]
                expression' e

expression' :: ParserM p => (Expression Int) -> p (Expression Int)
expression' e = return e
             \/ do token "+"
                   e' <- expr [mul, par, var, con]
                   expression' $ e :+: e'
             \/ do token "-"
                   e' <- expr [mul, par, var, con]
                   expression' $ e :-: e'

-- expr is a right-associative parser, of the expression types given by the
-- parsers in the argument.
expr :: ParserM p => [p (Expression Int)] -> p (Expression Int)
expr [p]    = p
expr (p:ps) = p \/ expr ps

-- TODO: negatieve getallen.
con :: ParserM p => p (Expression Int)
con = do x <- number
         return $ N x

var :: ParserM p => p (Expression Int)
var = do x <- variable
         return $ V x

par :: ParserM p => p (Expression Int)
par = do token "("
         x <- expr [mul, par, var, con]
         token ")"
         return x

mul :: ParserM p => p (Expression Int)
mul = do x <- expr [par, var, con]
         token "*"
         y <- expr [mul, par, var, con]
         return $ x :*: y

-- . Parse a condition.
-- `---------------------------------------------------------------------------

condition :: ParserM p => p (Condition Int)
condition = do x <- expression
               token ">"
               y <- expression
               return $ x :>: y

-- . Parse a variable.
-- `---------------------------------------------------------------------------

variable :: ParserM p => p Variable
variable = do c <- lower
              return [c]
        \/ do c <- lower
              cs <- variable
              return (c:cs)
