module Main where

import Tokens
import System.Environment
import Data.List

-- Operation definitions ---------------------------------------------------------------------------------
data StreamOp = Send | Copy | Print | Add | Multiply Int | Only Int| FromStream [Int] | Gen Int deriving (Eq, Show)
data TerminalStreamOp = ToEndStream Int | UndefinedEnd deriving (Eq, Show)
data StreamOpSequence = Op StreamOp StreamOpSequence | Combined StreamOpSequence StreamOpSequence | End TerminalStreamOp deriving (Eq, Show)

-- Input pre-processing ----------------------------------------------------------------------------------
extractLines :: String -> String -> [String]
extractLines ('\n':rest) building = building:(extractLines rest "")
extractLines (c:rest) building = extractLines rest (building ++ [c])
extractLines [] s = []

convertLine :: String -> String -> [Int]
convertLine (' ':rest) building = (read building :: Int):(convertLine rest "")
convertLine (s:"") building = [(read (building ++ [s]) :: Int)]
convertLine (s:rest) building = convertLine rest (building ++ [s])

numberise :: [String] -> [[Int]]
numberise (line:rest) = (convertLine line ""):(numberise rest)
numberise [] = []

formStreams :: String -> [[Int]]
formStreams input = transpose ( numberise (extractLines input "" ))

-- Token Processing -----------------------------------------------------------------------------------
skipToEndBracket :: [Token] -> [Token]
skipToEndBracket ((BClose):tokens) = tokens
skipToEndBracket (t:tokens) = skipToEndBracket tokens

skipToNextLine :: [Token] -> [Token]
skipToNextLine (((LineEnd):tokens)) = tokens
skipToNextLine (t:tokens) = skipToNextLine tokens

intakeToken :: [Token] -> [[Int]] -> [[Int]] -> IO [[Int]]
intakeToken (OutputOp:(DigitLit n):tokens) i o = produceOutput o n
intakeToken (InputStream:(DigitLit streamVal):tokens) i o = processToken tokens (i!!streamVal) i o
intakeToken (GenOp:(DigitLit n):tokens) i o = processToken tokens [n] i o
intakeToken (LastOp:(DigitLit n):tokens) i o = processToken tokens [last (o!!n)] i o
intakeToken (t:tokens) i o = intakeToken tokens i o
intakeToken [] i o = return o

processToken :: [Token] -> [Int] -> [[Int]] -> [[Int]] -> IO [[Int]]
processToken tokens source i o = do
    let opSequence = Op (FromStream source) (formStreamOpSequence tokens i)
    let (dest, vals) = processStreamOpSequence opSequence
    vals <- vals
    o <- pushValuesToStream vals o dest
    intakeToken (skipToNextLine tokens) i o

formStreamOpSequence :: [Token] -> [[Int]] -> StreamOpSequence
formStreamOpSequence ((SendOp):t) i = Op Send (formStreamOpSequence t i)
formStreamOpSequence ((ShowOp):t) i = Op Print (formStreamOpSequence t i)
formStreamOpSequence ((OutputStream):(DigitLit n):t) i = End (ToEndStream n)
formStreamOpSequence ((BClose):t) i = End UndefinedEnd
formStreamOpSequence ((CombineOp):(BOpen):t) i = (Combined opSequence (formStreamOpSequence tokens i)) where
                                            opSequence = formStreamOpSequence t i
                                            tokens = skipToEndBracket t
formStreamOpSequence ((AddOp):t) i = Op Add (formStreamOpSequence t i)
formStreamOpSequence ((MultiplyOp):(DigitLit n):t) i = Op (Multiply n) (formStreamOpSequence t i)
formStreamOpSequence ((OnlyOp):(DigitLit n):t) i = Op (Only n) (formStreamOpSequence t i)
formStreamOpSequence ((InputStream):(DigitLit streamVal):t) i = Op (FromStream (i!!streamVal)) (formStreamOpSequence t i)
formStreamOpSequence a i = End UndefinedEnd

-- Stream Processing -------------------------------------------------------------------------------------
streamDestination :: StreamOpSequence -> Int
streamDestination (Combined seq1 seq2) = streamDestination seq2
streamDestination (Op op seq) = streamDestination seq
streamDestination (End (ToEndStream n)) = n

processStreamOpSequence :: StreamOpSequence -> (Int, IO [Int])
processStreamOpSequence (Op (FromStream n) seq) = (streamDestination seq, recursiveProcess seq n 0)

recursiveProcess :: StreamOpSequence -> [Int] -> Int -> IO [Int]
recursiveProcess seq (val:inStream) count = do
    processedVal <- calculateStreamOpSequence seq val count
    if processedVal == 9223372036854775807 then --max int indicates error/abort
        return []
    else do
        nextVal <- recursiveProcess seq inStream (count+1)
        return (processedVal:nextVal)
recursiveProcess seq [] count = return []

calculateStreamOpSequence :: StreamOpSequence -> Int -> Int -> IO Int
calculateStreamOpSequence (Op Send seq) val count = calculateStreamOpSequence seq val count
calculateStreamOpSequence (Op (Only n) seq) val count
    | n > count = calculateStreamOpSequence seq val count
    | otherwise = return 9223372036854775807
calculateStreamOpSequence (End (ToEndStream n)) val count = return val
calculateStreamOpSequence (End UndefinedEnd) val count = do return val
calculateStreamOpSequence (Combined seq1 seq2) val count = processCombinedOp seq1 seq2 val count
calculateStreamOpSequence (Op (Multiply n) seq) val count = calculateStreamOpSequence seq (val*n) count
calculateStreamOpSequence (Op Print seq) val count = do putStr ((show val) ++ " "); calculateStreamOpSequence seq val count

processCombinedOp :: StreamOpSequence -> StreamOpSequence -> Int -> Int -> IO Int
processCombinedOp (Op (FromStream s1) seq1) (Op Send seq2) val count = do
    seq1Val <- calculateStreamOpSequence seq1 (s1!!count) count
    calculateCombinedOp seq2 seq1Val val count

calculateCombinedOp :: StreamOpSequence -> Int -> Int -> Int -> IO Int
calculateCombinedOp (Op Add seq) val1 val2 count = calculateStreamOpSequence seq (val1+val2) count

pushValuesToStream :: [Int] -> [[Int]] -> Int -> IO [[Int]]
pushValuesToStream values streams streamIndex = do
    let (start, stream:end) = splitAt streamIndex streams
    let newstream = stream ++ values
    return (start ++ (newstream:end))

produceOutput :: [[Int]] -> Int -> IO [[Int]]
produceOutput o numberofstreams = do
    let (trimmed, _) = splitAt numberofstreams o
    mapM_ (lineOutput) (transpose trimmed)
    return o

lineOutput :: [Int] -> IO ()
lineOutput (n:rest) = do putStr (show n); putStr " "; lineOutput rest
lineOutput [] = putStr "\n"

-- Main program -------------------------------------------------------------------------------------------
main :: IO ()
main = do
    (filename:debug) <- getArgs
    source <- readFile filename
    input <- getContents

    if debug == ["debug"] 
        then do
            putStrLn ("--- Loaded program ----------\n" ++ source)
            putStrLn ("--- Loaded inputs -----------\n" ++ input)
            putStrLn ("--- Starting Interpreter ----")
        else return ()

    let inputStreams = formStreams input
    let tokens = alexScanTokens source
    let outputStreams = replicate 100 []

    outputStreams <- intakeToken tokens inputStreams outputStreams
    return ()

