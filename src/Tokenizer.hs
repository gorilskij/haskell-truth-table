module Src.Tokenizer(tokenize) where

import Data.Char

import Src.Types

data Expecting = BOPC -- binary operator or paren close
               | Expr -- expression (variable, !, paren open)
               deriving Eq


isIdent :: Char -> Bool
-- isIdent = liftM2 (||) isAlpha (liftM2 (||) (('_') ==) (('-') ==)) -- too much
isIdent c = isAlpha c || c == '_' || c == '-'

--    parenDepth -> ...
tokenize' :: Int -> Expecting -> String -> [Token]
tokenize' d e (' ':s) = tokenize' d e s
tokenize' d e []
   | d > 0     = error "missing ')'"
   | d < 0     = error "unexpected ')'"
   | e == Expr = error "expected expression, found EOF"
   | otherwise = []

tokenize' d Expr ('(':s) = TParenOpen : tokenize' (d + 1) Expr s
tokenize' d BOPC (')':s) = TParenClose : tokenize' (d - 1) BOPC s
tokenize' d BOPC ('&':s) = TAnd : tokenize' d Expr s
tokenize' d BOPC ('|':s) = TOr : tokenize' d Expr s
tokenize' d Expr ('!':s) = TNot : tokenize' d Expr s
tokenize' d BOPC ('=':'>':s) = TImplies : tokenize' d Expr s
tokenize' d BOPC ('<':'=':'>':s) = TIff : tokenize' d Expr s

tokenize' d Expr s@(c:_)
    | isIdent c =
        let (var, rest) = splitAt (length (takeWhile isIdent s)) s in
            TVar var : tokenize' d BOPC rest

tokenize' _ _ (c:_) = error ("unexpected '" ++ c : "'")

tokenize :: String -> [Token]
tokenize = tokenize' 0 Expr
