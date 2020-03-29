module Src.Types(Token(..), Expression(..), toExpr) where

data Token = ParenOpen
           | ParenClose
           | And
           | Or
           | Not
           | Implies
           | Iff
           | Var String
           deriving (Show, Eq)

data Expression = Unparsed Token
                | Variable String
                | Op (Bool -> Bool) Expression
                | BinOp (Bool -> Bool -> Bool) Expression Expression

showParenWrapped :: Expression -> String
showParenWrapped b@(BinOp _ _ _) = "(" ++ show b ++ ")"
showParenWrapped e = show e

-- note this is work in progress and may output incorrectly
instance Show Expression where
    show (Unparsed t) = "Unparsed<" ++ show t ++ ">"
    show (Variable s) = s
    show (Op f e)
        | not (f True) = "!" ++ show e
    show (BinOp f l r)
        | f True False = showParenWrapped l ++ " | " ++ showParenWrapped r
        | not (f False False) = showParenWrapped l ++ " & " ++ showParenWrapped r
        | f False True = showParenWrapped l ++ " => " ++ showParenWrapped r
        | otherwise = showParenWrapped l ++ " <=> " ++ showParenWrapped r

toExpr :: [Token] -> [Expression]
toExpr = toExpr' . simplify where
    simplify (Not:Not:ts) = simplify ts
    simplify (t:ts) = t : simplify ts
    simplify [] = []

    toExpr' (Var s:ts) = Variable s : toExpr' ts
    toExpr' (t:ts) = Unparsed t : toExpr' ts
    toExpr' [] = []
