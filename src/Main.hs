module Main where

import Data.Char
import Data.Maybe (fromJust)

import Src.Tokenizer
import Src.Evaluator

main :: IO ()
main = do
    let s = "((a => b) & (b => a)) <=> (a <=> b)"
    putStrLn (table s)
    return ()
