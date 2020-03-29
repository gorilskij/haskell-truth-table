module Src.Types(
    Token(..),
    OpType(..),
    BinOpType(..),
    toFunc,
    toBinFunc,
    Expression(..),
    toExpr
    ) where

data Token = TParenOpen
           | TParenClose
           | TAnd
           | TOr
           | TNot
           | TImplies
           | TIff
           | TVar String
           deriving (Show, Eq)

data OpType = Not
data BinOpType = And | Or | Implies | Iff deriving Eq

-- TODO figure out how to do this with type classes
toFunc :: OpType -> (Bool -> Bool)
toFunc Not = not

toBinFunc :: BinOpType -> (Bool -> Bool -> Bool)
toBinFunc And = (&&)
toBinFunc Or = (||)
toBinFunc Implies = (||) . not
toBinFunc Iff = (==)

data Expression = Unparsed Token
                | Variable String
                | Op {
                    opType :: OpType,
                    expr :: Expression
                }
                | BinOp {
                    binOpType :: BinOpType,
                    left :: Expression,
                    right :: Expression
                }

showParenWrapped :: Expression -> String
showParenWrapped b@BinOp{} = "(" ++ show b ++ ")"
showParenWrapped e = show e

instance Show Expression where
    show (Unparsed t) = "Unparsed<" ++ show t ++ ">"
    show (Variable s) = s
    show Op{opType=Not, expr=expr} = "!" ++ show expr
    show BinOp{binOpType=opType, left=left, right=right}
        | opType == And
            = showParenWrapped left ++ " & " ++ showParenWrapped right
        | opType == Or
            = showParenWrapped left ++ " | " ++ showParenWrapped right
        | opType == Implies
            = showParenWrapped left ++ " => " ++ showParenWrapped right
        | opType == Iff
            = showParenWrapped left ++ " <=> " ++ showParenWrapped right

toExpr :: [Token] -> [Expression]
toExpr = toExpr' . simplify where
    simplify (TNot:TNot:ts) = simplify ts
    simplify (t:ts) = t : simplify ts
    simplify [] = []

    toExpr' (TVar s:ts) = Variable s : toExpr' ts
    toExpr' (t:ts) = Unparsed t : toExpr' ts
    toExpr' [] = []
