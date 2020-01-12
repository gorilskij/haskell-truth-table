module Types where

import Data.Map.Ordered

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

instance Show Expression where
    show (Unparsed t) = "U" ++ show t
    show (Variable s) = show s
    show (Op f e)
        | f True == False = "Not (" ++ show e ++ ")"
    show (BinOp f e ee)
        | f True False == True = "Or (" ++ show e ++ ", " ++ show ee ++ ")"
        | f False False == False = "And (" ++ show e ++ ", " ++ show ee ++ ")"
        | f False True == True = "Implies (" ++ show e ++ ", " ++ show ee ++ ")"
        | otherwise = "Iff (" ++ show e ++ ", " ++ show ee ++ ")"

toExpr :: [Token] -> [Expression]
toExpr = toExpr' . simplify where
    simplify (Not:Not:ts) = simplify ts
    simplify (t:ts) = t : simplify ts
    simplify [] = []

    toExpr' (Var s:ts) = Variable s : toExpr' ts
    toExpr' (t:ts) = Unparsed t : toExpr' ts
    toExpr' [] = []

type Variables = OMap String Bool
