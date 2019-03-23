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

processToken :: [Token] -> [[Int]] -> [[Int]] -> IO ()

processToken [] i o = putStrLn "Execution finished."

processToken (InputStream:(DigitLit streamVal):tokens) i o = do 
    putStrLn ("Loading stream:" ++ (show streamVal))
    let opSequence = formStreamOpSequence tokens
    putStrLn (show opSequence)
    recursiveProcess opSequence (i!!streamVal) o
    processToken tokens i o

processToken (t:tokens) i o = do
    putStrLn ("Skipping token:" ++ (show t))
    processToken tokens i o

formStreamOpSequence :: [Token] -> StreamOpSequence
formStreamOpSequence ((InputStream):(DigitLit n):t) = Op (FromStream n) (formStreamOpSequence t)
formStreamOpSequence ((SendOp):t) = Op Send (formStreamOpSequence t)
formStreamOpSequence ((CopyOp):t) = Op Copy (formStreamOpSequence t)
formStreamOpSequence ((ShowOp):t) = Op Print (formStreamOpSequence t)
formStreamOpSequence ((OutputStream):(DigitLit n):t) = End (ToEndStream n)
formStreamOpSequence a = End UndefinedEnd

processStreamOpSequence :: StreamOpSequence -> [Int] -> [[Int]] -> IO ()
processStreamOpSequence (Op (FromStream n) seq) i o = recursiveProcess seq i o
processStreamOpSequence seq i o = putStrLn ("Abort on:" ++ (show seq))

recursiveProcess :: StreamOpSequence -> [Int] -> [[Int]] -> IO ()
recursiveProcess seq (val:inStream) o = do
    calculateStreamOpSequence seq val
    recursiveProcess seq inStream o

recursiveProcess seq [] o = do
    putStrLn "Input stream exausted"

calculateStreamOpSequence :: StreamOpSequence -> Int -> IO ()
calculateStreamOpSequence (Op Send seq) val = calculateStreamOpSequence seq val

calculateStreamOpSequence (Op Print seq) val = do
    putStrLn (show val)
    calculateStreamOpSequence seq val

calculateStreamOpSequence (Op Copy seq) val = calculateStreamOpSequence seq val

calculateStreamOpSequence (End (ToEndStream n)) val = do
    putStrLn ("Append " ++ (show val) ++ " to output stream " ++ (show n))
calculateStreamOpSequence seq val = putStrLn ("Ended Stream sequence")

startInterpreting :: [Token] -> [[Int]] -> [[Int]] -> IO ()
startInterpreting tokens i o = processToken tokens i o

main :: IO ()
main = do
    args <- getArgs
    source <- readFile (head args)
    input <- getContents

    let inputStreams = formStreams input
    let outputStreams = []
    let tokens = alexScanTokens source

    startInterpreting tokens inputStreams outputStreams

