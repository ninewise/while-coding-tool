
module ParserM where

import Control.Monad
import Data.Char

newtype Parser a =
  P { runParser :: String -> [(a,String)] }

instance Monad Parser where
  return a = P $ \str -> [(a,str)]
  m >>= f  = P $ \str ->
    [  res  | (a,str')  <- runParser m str
            , res       <- runParser (f a) str'  ]

runP :: Parser a -> String -> Maybe a
runP p str =
  case  [x | (x,"") <- runParser p str ] of
    (x:_)  ->  Just x
    []     ->  Nothing

instance Functor Parser where
  fmap f p = p >>= return . f

class (Functor p, Monad p) => ParserM p where
  item   ::  p Char
  abort  ::  p a
  (\/)   ::  p a -> p a -> p a

instance ParserM Parser where
  item      = P $ \str  ->  case str of
                              (c:cs)  ->  [(c,cs)]
                              []      ->  []
  abort     = P $ \_    ->  []
  p1 \/ p2  = P $ \str  ->  runParser p1 str ++ runParser p2 str

satisfy :: ParserM p => (a -> Bool) -> p a -> p a
satisfy f p = p >>= \a -> if f a then return a else abort

char :: ParserM p => Char -> p Char
char c = satisfy (==c) item

lower :: ParserM p => p Char
lower = satisfy isLower item

digit :: ParserM p => p Int
digit = fmap digitToInt $ satisfy isDigit item

number :: ParserM p => p Int
number = digit >>= go
  where
    go acc = (digit >>= \d -> go (acc * 10 + d)) \/ return acc 
