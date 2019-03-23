module Main where

import Tokens

import Tokens
import System.Environment
import Data.Char(digitToInt)
import Data.List

-- Operation definitions ---------------------------------------------------------------------------------
data StreamOp = Send | Copy | Print | Add | FromStream [Int] | Gen Int deriving (Eq, Show)
data TerminalStreamOp = ToEndStream Int | CachedEnd [Int] | UndefinedEnd deriving (Eq, Show)
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

processToken :: [Token] -> [[Int]] -> [[Int]] -> IO ()

processToken [] i o = putStrLn "\n\nExecution finished."

processToken (InputStream:(DigitLit streamVal):tokens) i o = do 
    let opSequence = Op (FromStream (i!!streamVal)) (formStreamOpSequence tokens i)
    --putStr ("InputStream" ++ (show streamVal))
    processStreamOpSequence opSequence i o
    processToken tokens i o

processToken (GenOp:(DigitLit n):tokens) i o = do
    let opSequence = Op (FromStream [n]) (formStreamOpSequence tokens i)
    processStreamOpSequence opSequence i o
    processToken tokens i o

processToken (t:tokens) i o = do
    processToken tokens i o

-- Stream Processing -------------------------------------------------------------------------------------
formStreamOpSequence :: [Token] -> [[Int]] -> StreamOpSequence
formStreamOpSequence ((SendOp):t) i = Op Send (formStreamOpSequence t i)
formStreamOpSequence ((ShowOp):t) i = Op Print (formStreamOpSequence t i)
formStreamOpSequence ((OutputStream):(DigitLit n):t) i = End (ToEndStream n)
formStreamOpSequence ((BClose):t) i = End (CachedEnd [])
formStreamOpSequence ((CombineOp):(BOpen):t) i = (Combined opSequence (formStreamOpSequence tokens i)) where
                                            opSequence = formStreamOpSequence t i
                                            tokens = skipToEndBracket t
formStreamOpSequence ((AddOp):t) i = Op Add (formStreamOpSequence t i)
formStreamOpSequence ((InputStream):(DigitLit streamVal):t) i = Op (FromStream (i!!streamVal)) (formStreamOpSequence t i)
formStreamOpSequence a i = End UndefinedEnd

processStreamOpSequence :: StreamOpSequence -> [[Int]] -> [[Int]] -> IO Int
processStreamOpSequence (Op (FromStream n) seq) i o = recursiveProcess seq n o 0

recursiveProcess :: StreamOpSequence -> [Int] -> [[Int]] -> Int -> IO Int
recursiveProcess seq (val:inStream) o count = do
    calculateStreamOpSequence seq val o count
    recursiveProcess seq inStream o (count+1)

recursiveProcess seq [] o count = do
    return 0

calculateStreamOpSequence :: StreamOpSequence -> Int -> [[Int]] -> Int -> IO Int
calculateStreamOpSequence (Op Send seq) val o count = do
    --putStr ("->")
    calculateStreamOpSequence seq val o count

calculateStreamOpSequence (Op Print seq) val o count = do
    putStrLn (show val)
    calculateStreamOpSequence seq val o count

calculateStreamOpSequence (End (ToEndStream n)) val o count = do
    putStrLn ((show val) ++ "->OutputStream" ++ (show n))
    return val

calculateStreamOpSequence (End (CachedEnd n)) val o count = do 
    return val

calculateStreamOpSequence (Combined seq1 seq2) val o count = do
    --putStr (">>")
    calculateCombinedOp seq1 seq2 val o count

calculateStreamOpSequence (Op Add seq) val o count = do
    calculateStreamOpSequence seq val o count

calculateStreamOpSequence seq val o count = do 
    --putStrLn (";")
    return val

calculateCombinedOp :: StreamOpSequence -> StreamOpSequence -> Int -> [[Int]] -> Int -> IO Int
calculateCombinedOp (Op (FromStream s1) seq1) seq2 val o count = do
    seq1Val <- calculateStreamOpSequence seq1 (s1!!count) o count
    --putStr ((show val) ++ "+" ++ (show seq1Val))
    calculateStreamOpSequence seq2 (val+seq1Val) o count
    

-- Main program -------------------------------------------------------------------------------------------
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

