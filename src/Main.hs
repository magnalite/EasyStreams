module Main where

import Tokens

import Tokens
import System.Environment
import Data.Char(digitToInt)
import Data.List

extractLines :: String -> String -> [String]
extractLines ('\n':rest) building = building:(extractLines rest "")
extractLines (c:rest) building = extractLines rest (building ++ [c])
extractLines [] s = []

convertLine :: String -> String -> [Int]
convertLine (' ':rest) building = (read building :: Int):(convertLine rest "")
convertLine (s:"") building = [(read (s:building) :: Int)]
convertLine (s:rest) building = convertLine rest (s:building)

numberise :: [String] -> [[Int]]
numberise (line:rest) = (convertLine line ""):(numberise rest)
numberise [] = []

--Reads in stream inputs and returns a list of int arrays
--Eg 
-- 3 5 4
-- 2 3 1 -> [[3, 2, 6], [5, 3, 3], [4, 1, 7]]
-- 6 3 7
formStreams :: String -> [[Int]]
formStreams input = transpose ( numberise (extractLines input "" ))

main :: IO ()
main = do
    args <- getArgs
    source <- readFile (head args)
    input <- getContents
    let inputStreams = formStreams input

    putStrLn (show inputStreams)

    --let tokens = alexScanTokens source
    --putStrLn (show tokens)
    let firstInputs = (inputStreams!!0)
    putStrLn (show firstInputs)
    let secondInputs = (inputStreams!!2)
    putStrLn (show secondInputs)
