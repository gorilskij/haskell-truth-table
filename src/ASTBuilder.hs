module ASTBuilder(buildAST) where

import Types

-- assumes that opening paren has been removed
lowerParens' :: [Expression] -> [Expression] -> [Expression]
lowerParens' g (Unparsed TParenClose:es) = buildAST g : es
lowerParens' g (Unparsed TParenOpen:es) = lowerParens' (g ++ [head lp]) (tail lp) where
    lp = lowerParens' [] es
lowerParens' g (e:es) = lowerParens' (g ++ [e]) es
lowerParens' _ [] = error "missing ')'"

lowerParens :: [Expression] -> [Expression]
lowerParens (Unparsed TParenOpen:es) = head lp : lowerParens (tail lp) where
    lp = lowerParens' [] es
lowerParens (Unparsed TParenClose:_) = error "unexpected ')'"
lowerParens (e:es) = e : lowerParens es
lowerParens [] = []

-- TODO: combine the following functions somehow
lowerNot :: [Expression] -> [Expression]
lowerNot (Unparsed TNot:e:es) = Op Not e : lowerNot es
lowerNot (e:es) = e : lowerNot es
lowerNot [] = []

lowerAnd (e:Unparsed TAnd:f:es) = BinOp And e f : lowerAnd es
lowerAnd (e:es) = e : lowerAnd es
lowerAnd [] = []

lowerOr (e:Unparsed TOr:f:es) = BinOp Or e f : lowerOr es
lowerOr (e:es) = e : lowerOr es
lowerOr [] = []

lowerImplies (e:Unparsed TImplies:f:es) = BinOp Implies e f : lowerImplies es
lowerImplies (e:es) = e : lowerImplies es
lowerImplies [] = []

lowerIff (e:Unparsed TIff:f:es) = BinOp Iff e f : lowerIff es
lowerIff (e:es) = e : lowerIff es
lowerIff [] = []

buildAST' = lowerIff . lowerImplies . lowerOr . lowerAnd . lowerNot . lowerParens

buildAST :: [Expression] -> Expression
buildAST ts
    | length astList == 1 = head astList
    | otherwise           = error ("unexpected astList with length " ++ show (length astList) ++ "\n" ++ show astList)
    where astList = buildAST' ts
