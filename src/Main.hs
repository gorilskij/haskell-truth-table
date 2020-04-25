module Main where

import Data.Char
import Data.Maybe (fromJust)

import Src.Tokenizer
import Src.Evaluator

main :: IO ()
main = do
    let s = "((a => b) & (b => a)) <=> (a <=> b)"
--    let s = "a & b | c & you_can_have_long_names_too => with-underscores-or-dashes"
    putStrLn (table s)
    return ()
