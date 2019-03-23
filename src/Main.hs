module Main where

import Tokens

import Tokens
import System.Environment
import Data.Char(digitToInt)


--Reads in one line at a time and splits into seperate streams
--Eg 3 5 4 -> [3, 5, 4]
extractStreamLine :: String -> [Int]
extractStreamLine (' ':rest) = [] ++ (extractStreamLine rest)
extractStreamLine (num:rest) = [digitToInt num] ++ (extractStreamLine rest)
extractStreamLine [] = []

fetchStreamLine :: IO [Int]
fetchStreamLine = do 
    line <- getLine
    putStrLn ("Fetched:" ++ line)
    return (extractStreamLine line)

--Reads in stream inputs and returns a list of int arrays
--Eg 
-- 3 5 4
-- 2 3 1 -> [[3, 5, 4], [2, 3, 1], [6, 3, 7]]
-- 6 3 7
formStreams :: [IO [Int]]
formStreams = fetchStreamLine:formStreams

main :: IO ()
main = do
    args <- getArgs
    source <- readFile (head args)
    let inputStreams = formStreams

    let tokens = alexScanTokens source
    putStrLn (show tokens)
    firstInputs <- (inputStreams!!1)
    putStrLn (show firstInputs)
    secondInputs <- (inputStreams!!2)
    putStrLn (show secondInputs)
