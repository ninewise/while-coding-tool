
module WhileParser where

import Prelude hiding (sequence)
import AbstractSyntaxTree
import ParserM

parseProgram :: String -> Program
parseProgram s = case runP program s of
    Just x  -> x
    Nothing -> error "Failed to parse program."

-- . Additional functions.
-- `---------------------------------------------------------------------------

-- parses a random amount of spaces, newlines and tabs.
whitespace :: ParserM p => p ()
whitespace = return ()
          \/ do (char ' ' \/ char '\n' \/ char '\t')
                whitespace
                return ()

-- parses the given string, surrounded by optional whitespace.
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

-- parses several instances of the given parser, seperated by a komma,
-- surrounded by optional whitepsace.
list :: ParserM p => p a -> p [a]
list m = do x <- m
            return [x]
      \/ do x <- m
            token ","
            xs <- list m
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

-- A statement consists of two statements seprated by a semicolon, an
-- assignment, a while loop, a function or a comment followed by a statement.
statement :: ParserM p => p Statement
statement = sequence \/ assignment \/ loop \/ function \/ (comment >>= \_ -> statement)

assignment :: ParserM p => p Statement
assignment = do v <- variable
                token "="
                e <- expression
                return $ v :=: e

loop :: ParserM p => p Statement
loop = do token "while"
          token "("
          c <- condition
          token ")"
          token "{"
          s <- statement
          token "}"
          return $ While c s

function :: ParserM p => p Statement
function = do token "function"
              n <- variable
              token "("
              ps <- list variable
              token ")"
              token "->"
              r <- variable
              token "{"
              s <- statement
              token "}"
              return $ Funct n r ps s

sequence :: ParserM p => p Statement
sequence = do x <- (assignment \/ loop \/ function)
              token ";"
              y <- statement
              return $ x ::: y

-- comments of the form /*   */ can be placed in front of each
-- statement, possibly on a seperate line. They cannot be placed in the
-- middle of a statement.
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
expression = do e <- expr [mul, par, fun, var, con]
                expression' e

expression' :: ParserM p => (Expression Int) -> p (Expression Int)
expression' e = return e
             \/ do token "+"
                   e' <- expr [mul, par, fun, var, con]
                   expression' $ e :+: e'
             \/ do token "-"
                   e' <- expr [mul, par, fun, var, con]
                   expression' $ e :-: e'

-- expr is a right-associative parser, of the expression types given by the
-- parsers in the argument.
expr :: ParserM p => [p (Expression Int)] -> p (Expression Int)
expr [p]    = p
expr (p:ps) = p \/ expr ps

con :: ParserM p => p (Expression Int)
con = do x <- number
         return $ N x

var :: ParserM p => p (Expression Int)
var = do x <- variable
         return $ V x

fun :: ParserM p => p (Expression Int)
fun = do x <- variable
         token "("
         as <- list expression
         token ")"
         return $ C x as

par :: ParserM p => p (Expression Int)
par = do token "("
         x <- expression
         token ")"
         return x

mul :: ParserM p => p (Expression Int)
mul = do x <- expr [par, fun, var, con]
         token "*"
         y <- expr [mul, par, fun, var, con]
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
