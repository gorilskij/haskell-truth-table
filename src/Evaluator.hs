module Src.Evaluator(table) where

import Data.Map.Ordered
import Data.Maybe (fromJust)

import Src.Types
import Src.Tokenizer
import Src.ASTBuilder

type Variables = OMap String Bool

getVariables :: [Token] -> Variables
getVariables = getVariables' empty where
    getVariables' :: Variables -> [Token] -> Variables
    getVariables' ss (TVar s:ts) = getVariables' (ss >| (s, False)) ts
    getVariables' ss (_:ts) = getVariables' ss ts
    getVariables' ss [] = ss

parseString :: String -> (Variables, Expression)
parseString s = let t = tokenize s in
    (getVariables t, buildAST $ toExpr t)

evaluate :: Variables -> Expression -> Bool
evaluate vs (Variable s) = fromJust $ Data.Map.Ordered.lookup s vs
evaluate vs (Op o e) = toFunc o (evaluate vs e)
evaluate vs (BinOp o e f) = toBinFunc o (evaluate vs e) (evaluate vs f)

increment :: Variables -> Variables
increment = fromList . reverse . increment' . reverse . assocs where
    increment' ((k, False):kv) = (k, True) : kv
    increment' ((k, True):kv) = (k, False) : increment' kv

doTitle :: Variables -> Expression -> (String, [Int])
doTitle vs e = (unwords keys ++ " - " ++ show e, map length keys) where
    keys = map fst (assocs vs)

-- pads with "  -  -  -..."
rPad :: Int -> String -> String
rPad 0 _ = []
rPad l (x:xs) = x : rPad (l - 1) xs
rPad l [] = take l (unwords (repeat "  -"))

doLine :: Variables -> Expression -> [Int] -> String
doLine vs e ps = unwords padded ++ " - " ++ b2s (evaluate vs e) where
    b2s True = "T"
    b2s False = "F"

    vars = map (b2s . snd) (assocs vs)
    padded = zipWith rPad ps vars

-- checks if all variables are currently set to True, used to detect when to
-- stop adding lines to a table (start all false, finish all true)
allTrue :: Variables -> Bool
allTrue vs = all snd (assocs vs)

doLines :: Variables -> Expression -> [Int] -> String
doLines vs e ps
    | allTrue vs = doLine vs e ps
    | otherwise  = doLine vs e ps ++ "\n" ++ doLines (increment vs) e ps

doTable :: Variables -> Expression -> String
doTable vs e = title ++ "\n" ++ doLines vs e padding where
    (title, padding) = doTitle vs e

table :: String -> String
table = uncurry doTable . parseString
