module Evaluator(table) where

import Data.Map.Ordered
import Data.Maybe (fromJust)

import Types(Token(Var), Expression(Variable, Op, BinOp), toExpr)
import Tokenizer(tokenize)
import ASTBuilder(buildAST)

type Variables = OMap String Bool

getVariables :: [Token] -> Variables
getVariables = getVariables' empty where
    getVariables' :: Variables -> [Token] -> Variables
    getVariables' ss (Var s:ts) = getVariables' (ss >| (s, False)) ts
    getVariables' ss (_:ts) = getVariables' ss ts
    getVariables' ss [] = ss

parseString :: String -> (Variables, Expression)
parseString s = let t = tokenize s in (getVariables t, buildAST . toExpr $ t)

evaluate :: Variables -> Expression -> Bool
evaluate vs (Variable s) = fromJust $ Data.Map.Ordered.lookup s vs
evaluate vs (Op o e) = o (evaluate vs e)
evaluate vs (BinOp o e f) = o (evaluate vs e) (evaluate vs f)

increment :: Variables -> Variables
increment = fromList . reverse . increment' . reverse . assocs where
    increment' ((k, False):kv) = (k, True) : kv
    increment' ((k, True):kv) = (k, False) : increment' kv

doTitle :: Variables -> Expression -> (String, [Int])
doTitle vs e = (unwords keys ++ " - " ++ show e, map length keys) where
    keys = map fst (assocs vs)

rPad :: a -> Int -> [a] -> [a]
rPad _ 0 _ = []
rPad p l (x:xs) = x : rPad p (l - 1) xs
rPad p l [] = p : rPad p (l - 1) []

doLine :: Variables -> Expression -> [Int] -> String
doLine vs e ps = unwords padded ++ " - " ++ b2s (evaluate vs e) where
    b2s True = "T"
    b2s False = "F"

    vars = map (b2s . snd) (assocs vs)
    padded = map (uncurry (rPad ' ')) (zip ps vars)

allTrue :: Variables -> Bool
allTrue vs = and (map snd (assocs vs))

doLines :: Variables -> Expression -> [Int] -> String
doLines vs e ps
    | allTrue vs = doLine vs e ps
    | otherwise  = doLine vs e ps ++ "\n" ++ doLines (increment vs) e ps

doTable :: Variables -> Expression -> String
doTable vs e = title ++ "\n" ++ doLines vs e padding where
    (title, padding) = doTitle vs e

table :: String -> String
table = uncurry doTable . parseString
