module Main where

import Tokens

import Tokens
import System.Environment
import Data.Char(digitToInt)
import Data.List

-- Operation definitions ---------------------------------------------------------------------------------
data StreamOp = Send | Copy | Print | Add | FromStream [Int] | Gen Int deriving (Eq, Show)
data TerminalStreamOp = ToEndStream Int | UndefinedEnd deriving (Eq, Show)
data StreamOpSequence = Op StreamOp StreamOpSequence | Combined StreamOpSequence StreamOpSequence | End TerminalStreamOp deriving (Eq, Show)

-- Input pre-processing ----------------------------------------------------------------------------------
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

formStreams :: String -> [[Int]]
formStreams input = transpose ( numberise (extractLines input "" ))


-- Token Processing -------------------------------------------------------------------------------------
skipToEndBracket :: [Token] -> [Token]
skipToEndBracket ((BClose):tokens) = tokens
skipToEndBracket (t:tokens) = skipToEndBracket tokens

skipToNextLine :: [Token] -> [Token]
skipToNextLine (((LineEnd):tokens)) = tokens
skipToNextLine (t:tokens) = skipToNextLine tokens

processToken :: [Token] -> [[Int]] -> IO ()

processToken [] i = putStrLn "\n\nExecution finished."

processToken (InputStream:(DigitLit streamVal):tokens) i = do 
    let opSequence = Op (FromStream (i!!streamVal)) (formStreamOpSequence tokens i)
    endVal <- processStreamOpSequence opSequence
    putStrLn ("Ops returned: " ++ (show endVal))
    processToken (skipToNextLine tokens) i

processToken (GenOp:(DigitLit n):tokens) i = do
    let opSequence = Op (FromStream [n]) (formStreamOpSequence tokens i)
    let vals = processStreamOpSequence opSequence
    processToken tokens i

processToken (t:tokens) i = do
    processToken tokens i

-- Stream Processing -------------------------------------------------------------------------------------
formStreamOpSequence :: [Token] -> [[Int]] -> StreamOpSequence
formStreamOpSequence ((SendOp):t) i = Op Send (formStreamOpSequence t i)
formStreamOpSequence ((ShowOp):t) i = Op Print (formStreamOpSequence t i)
formStreamOpSequence ((OutputStream):(DigitLit n):t) i = End (ToEndStream n)
formStreamOpSequence ((BClose):t) i = End UndefinedEnd
formStreamOpSequence ((CombineOp):(BOpen):t) i = (Combined opSequence (formStreamOpSequence tokens i)) where
                                            opSequence = formStreamOpSequence t i
                                            tokens = skipToEndBracket t
formStreamOpSequence ((AddOp):t) i = Op Add (formStreamOpSequence t i)
formStreamOpSequence ((InputStream):(DigitLit streamVal):t) i = Op (FromStream (i!!streamVal)) (formStreamOpSequence t i)
formStreamOpSequence a i = End UndefinedEnd

processStreamOpSequence :: StreamOpSequence -> IO [Int]
processStreamOpSequence (Op (FromStream n) seq) = recursiveProcess seq n 0

recursiveProcess :: StreamOpSequence -> [Int] -> Int -> IO [Int]
recursiveProcess seq (val:inStream) count = do
    processedVal <- calculateStreamOpSequence seq val count
    nextVal <- recursiveProcess seq inStream (count+1)
    return (processedVal:nextVal)
recursiveProcess seq [] count = return []

calculateStreamOpSequence :: StreamOpSequence -> Int -> Int -> IO Int
calculateStreamOpSequence (Op Send seq) val count = calculateStreamOpSequence seq val count
calculateStreamOpSequence (End (ToEndStream n)) val count = do return val
calculateStreamOpSequence (End UndefinedEnd) val count = do return val
calculateStreamOpSequence (Combined seq1 seq2) val count = processCombinedOp seq1 seq2 val count
calculateStreamOpSequence (Op Print seq) val count = do
    putStrLn (show val)
    calculateStreamOpSequence seq val count
calculateStreamOpSequence seq val count = do 
    putStrLn ("Unhandled sequence:" ++ (show seq))
    return val

processCombinedOp :: StreamOpSequence -> StreamOpSequence -> Int -> Int -> IO Int
processCombinedOp (Op (FromStream s1) seq1) (Op Send seq2) val count = do
    seq1Val <- calculateStreamOpSequence seq1 (s1!!count) count
    calculateCombinedOp seq2 seq1Val val count

calculateCombinedOp :: StreamOpSequence -> Int -> Int -> Int -> IO Int
calculateCombinedOp (Op Add seq) val1 val2 count = calculateStreamOpSequence seq (val1+val2) count


-- Main program -------------------------------------------------------------------------------------------
startInterpreting :: [Token] -> [[Int]] -> [[Int]] -> IO ()
startInterpreting tokens i o = processToken tokens i

main :: IO ()
main = do
    args <- getArgs
    source <- readFile (head args)
    input <- getContents

    let inputStreams = formStreams input
    let outputStreams = []
    let tokens = alexScanTokens source

    startInterpreting tokens inputStreams outputStreams

