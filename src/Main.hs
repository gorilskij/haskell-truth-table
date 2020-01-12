module Main where

import Data.Char
import Data.Map.Ordered
import Data.Maybe (fromJust)

import Src.Tokenizer
import Src.Evaluator

main :: IO ()
main = do
    let s = "a & b | c"
    putStrLn (table s)
    return ()
