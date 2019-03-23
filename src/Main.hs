module Main where

import Tokens

import Tokens
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    source <- readFile (head args)
    input <- getContents
    
    let tokens = alexScanTokens source
    putStrLn (show tokens)
    putStrLn (show input)

