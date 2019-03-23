module Main where

import Tokens

main :: IO ()
main = do
    s <- getContents
    putStrLn(s)
    let tokens = alexScanTokens s 
    putStrLn(show tokens)

