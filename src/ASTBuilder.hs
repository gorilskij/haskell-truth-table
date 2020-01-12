module Src.ASTBuilder(buildAST) where

import Src.Types

-- assumes that opening paren has been removed
lowerParens' :: [Expression] -> [Expression] -> [Expression]
lowerParens' g (Unparsed ParenClose:es) = buildAST g : es
lowerParens' g (Unparsed ParenOpen:es) = lowerParens' (g ++ [head lp]) (tail lp) where
    lp = lowerParens' [] es
lowerParens' g (e:es) = lowerParens' (g ++ [e]) es
lowerParens' _ [] = error "missing ')'"

lowerParens :: [Expression] -> [Expression]
lowerParens (Unparsed ParenOpen:es) = head lp : lowerParens (tail lp) where
    lp = lowerParens' [] es
lowerParens (Unparsed ParenClose:_) = error "unexpected ')'"
lowerParens (e:es) = e : lowerParens es
lowerParens [] = []

-- TODO: combine the following functions somehow
lowerNot (Unparsed Not:e:es) = Op not e : lowerNot es
lowerNot (e:es) = e : lowerNot es
lowerNot [] = []

lowerAnd (e:Unparsed And:f:es) = BinOp (&&) e f : lowerAnd es
lowerAnd (e:es) = e : lowerAnd es
lowerAnd [] = []

lowerOr (e:Unparsed Or:f:es) = BinOp (||) e f : lowerOr es
lowerOr (e:es) = e : lowerOr es
lowerOr [] = []

lowerImplies (e:Unparsed Implies:f:es) = BinOp ((||) . not) e f : lowerImplies es
lowerImplies (e:es) = e : lowerImplies es
lowerImplies [] = []

lowerIff (e:Unparsed Iff:f:es) = BinOp (==) e f : lowerIff es
lowerIff (e:es) = e : lowerIff es
lowerIff [] = []

buildAST' = lowerIff . lowerImplies . lowerOr . lowerAnd . lowerNot . lowerParens
-- buildAST' = lowerImplies . lowerAnd . lowerParens

buildAST :: [Expression] -> Expression
buildAST ts
    | length astList == 1 = head astList
    | otherwise           = error ("unexpected astList with length " ++ show (length astList) ++ "\n" ++ show astList)
    where astList = buildAST' ts
