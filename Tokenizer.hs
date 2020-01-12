module Main where

import Data.Char
import Data.Map.Ordered
import Data.Maybe (fromJust)

----- TOKENIZER -----
data Token = ParenOpen
           | ParenClose
           | And
           | Or
           | Not
           | Implies
           | Iff
           | Var String
           deriving (Show, Eq)

data Expecting = BOPC -- binary operator or paren close
               | Expr -- expression (variable, !, paren open)
               deriving Eq

tokenize :: String -> [Token]
tokenize = tokenize' 0 Expr where
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
----- TOKENIZER -----

----- AST BUILDER -----
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

buildAST :: [Expression] -> Expression
buildAST ts
    | length astList == 1 = head astList
    | otherwise           = error ("unexpected astList with length " ++ show (length astList))
    where
        astList = buildAST' ts
        buildAST' ts = lowerIff . lowerImplies . lowerOr . lowerAnd . lowerNot . lowerParens $ ts where
            lowerParens :: [Expression] -> [Expression]
            lowerParens (Unparsed ParenOpen:es) = lowerParens' [] es where
                lowerParens' :: [Expression] -> [Expression] -> [Expression]
                lowerParens' ps (Unparsed ParenOpen:es) = lowerParens' (ps ++ lp) rest where
                    lp = lowerParens' [] es
                    rest = drop (length lp) es
                lowerParens' p (Unparsed ParenClose:es) = (buildAST p) : es
                lowerParens' p (e:es) = lowerParens' (p ++ [e]) es
            lowerParens (e:es) = e : lowerParens es
            lowerParens [] = []

            lowerNot (Unparsed Not:e:es) = Op not e : lowerNot es
            lowerNot (e:es) = e : lowerNot es
            lowerNot [] = []

            lowerAnd (e:Unparsed And:f:es) = BinOp(&&) e f : lowerAnd es
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
----- AST BUILDER -----

----- EVALUATION -----
type Variables = OMap String Bool

getVariables :: [Token] -> Variables
getVariables = getVariables' empty where
    getVariables' :: Variables -> [Token] -> Variables
    getVariables' ss (Var s:ts) = getVariables' ((s, False) |< ss) ts
    getVariables' ss (_:ts) = getVariables' ss ts
    getVariables' ss [] = ss

table :: String -> String
table = uncurry doTable . parseString where
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
----- EVALUATION -----

main :: IO ()
main = do
    let s = " a <=> (b&!!coo | !k) "
    putStrLn (table s)
    return ()
