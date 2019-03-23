module Main where

import Tokens

import Tokens
import System.Environment
import Data.Char(digitToInt)
import Data.List

data StreamOp = Send | Copy | Print | Add | FromStream Int deriving (Eq, Show)
data TerminalStreamOp = ToEndStream Int | UndefinedEnd deriving (Eq, Show)
data StreamOpSequence = Op StreamOp StreamOpSequence | End TerminalStreamOp deriving (Eq, Show)

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

--getStreamValue :: [IO [Int]] -> Int -> Int -> IO Int
--getStreamValue i streamVal index = 

processNextToken :: [Token] -> [IO [Int]] -> [[Int]] -> Int -> IO ()
processNextToken t i o pos = processToken (t!!pos) t i o pos

processToken :: Token -> [Token] -> [IO [Int]] -> [[Int]] -> Int -> IO ()

processToken InputStream tokens i o pos = do 
    let (DigitLit streamVal) = (tokens!!(pos+1))
    putStrLn ("Loading stream:" ++ (show streamVal))
    let opSequence = formStreamOpSequence (drop pos tokens)
    putStrLn (show opSequence)
    processStreamOpSequence opSequence i o 1
    processNextToken tokens i o (pos+2)

processToken t tokens i o pos = do
    --putStrLn ("Skipping token:" ++ (show t))
    processNextToken tokens i o (pos+1)

formStreamOpSequence :: [Token] -> StreamOpSequence
formStreamOpSequence ((InputStream):(DigitLit n):t) = Op (FromStream n) (formStreamOpSequence t)
formStreamOpSequence ((SendOp):t) = Op Send (formStreamOpSequence t)
formStreamOpSequence ((CopyOp):t) = Op Copy (formStreamOpSequence t)
formStreamOpSequence ((ShowOp):t) = Op Print (formStreamOpSequence t)
formStreamOpSequence ((OutputStream):(DigitLit n):t) = End (ToEndStream n)
formStreamOpSequence a = End UndefinedEnd

processStreamOpSequence :: StreamOpSequence -> [IO [Int]] -> [[Int]] -> Int -> IO ()
processStreamOpSequence (Op (FromStream n) seq) i o index = do
    putStrLn ("INDEXING: " ++ (show index))
    streamLine <- (head i)
    let initialVal = streamLine!!n
    calculateStreamOpSequence seq initialVal
    
calculateStreamOpSequence :: StreamOpSequence -> Int -> IO ()
calculateStreamOpSequence (Op Send seq) val = calculateStreamOpSequence seq val

calculateStreamOpSequence (Op Print seq) val = do
    putStrLn (show val)
    calculateStreamOpSequence seq val

calculateStreamOpSequence (Op Copy seq) val = calculateStreamOpSequence seq val

calculateStreamOpSequence (End (ToEndStream n)) val = do
    putStrLn ("Append" ++ (show val) ++ " to output stream " ++ (show n))
calculateStreamOpSequence seq val = putStrLn ("Ended Stream sequence")

startInterpreting :: [Token] -> [IO [Int]] -> [[Int]] -> IO ()
startInterpreting tokens i o = processNextToken tokens i o 0

main :: IO ()
main = do
    args <- getArgs
    source <- readFile (head args)

    let inputStreams = formStreams
    let outputStreams = []
    let tokens = alexScanTokens source

    startInterpreting tokens inputStreams outputStreams

