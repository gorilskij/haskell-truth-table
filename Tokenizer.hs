module Tokenizer(tokenize) where

import Data.Char

import Types(Token(Var, ParenOpen, ParenClose, And, Or, Not, Implies, Iff))

data Expecting = BOPC -- binary operator or paren close
               | Expr -- expression (variable, !, paren open)
               deriving Eq

--    parenDepth -> ...
tokenize' :: Int -> Expecting -> String -> [Token]
tokenize' d e (' ':s) = tokenize' d e s
tokenize' d e []
   | d > 0     = error "missing ')'"
   | d < 0     = error "unexpected ')'"
   | e == Expr = error "expected expression, got end"
   | otherwise = []

tokenize' d Expr ('(':s) = ParenOpen : tokenize' (d + 1) Expr s
tokenize' d BOPC (')':s) = ParenClose : tokenize' (d - 1) BOPC s
tokenize' d BOPC ('&':s) = And : tokenize' d Expr s
tokenize' d BOPC ('|':s) = Or : tokenize' d Expr s
tokenize' d Expr ('!':s) = Not : tokenize' d Expr s
tokenize' d BOPC ('=':'>':s) = Implies : tokenize' d Expr s
tokenize' d BOPC ('<':'=':'>':s) = Iff : tokenize' d Expr s

tokenize' d Expr s@(c:_)
   | isAlpha c = let (var, rest) = splitAt (length (takeWhile isAlpha s)) s in Var var : tokenize' d BOPC rest

tokenize' _ _ (c:_) = error ("unexpected '" ++ c : "'")

tokenize :: String -> [Token]
tokenize = tokenize' 0 Expr where
