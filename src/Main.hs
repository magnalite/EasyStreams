module Main where

import Tokens
import System.Environment
import Data.List

-- Operation definitions ---------------------------------------------------------------------------------
data StreamOp = Send | Copy | Print | Add | Sub | Negate | Mod | Pow | Multiply Int | Divide Int | CombinedMultiply | CombinedDivide | Only Int| FromStream [Int] | Last Int | Skip Int | FromLast Int Int | Gen Int deriving (Eq, Show)
data TerminalStreamOp = ToEndStream Int | UndefinedEnd deriving (Eq, Show)
data StreamOpSequence = Op StreamOp StreamOpSequence | Combined StreamOpSequence StreamOpSequence | End TerminalStreamOp deriving (Eq, Show)
data StreamInfo = Info [[Int]] [[Int]] --Instreams outstreams

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

intakeToken :: [Token] -> StreamInfo -> IO StreamInfo
intakeToken (OutputOp:(DigitLit n):tokens) info = produceOutput info n
intakeToken (InputStream:(DigitLit streamVal):tokens) (Info i o) = processToken tokens (i!!streamVal) (Info i o)
intakeToken (GenOp:(DigitLit n):tokens) (Info i o) = processToken tokens [n] (Info i o)
intakeToken (FromLastOp:(DigitLit sn):(DigitLit n):tokens) (Info i o) = do
    let stream = o!!n
    let (_, value:_) = splitAt ((length stream)-(sn+1)) stream
    processToken tokens [value] (Info i o)
intakeToken (t:tokens) (Info i o) = do putStrLn ("Skipped" ++ (show t)); intakeToken tokens (Info i o)
intakeToken [] (Info i o) = return (Info i o)

processToken :: [Token] -> [Int] -> StreamInfo -> IO StreamInfo
processToken tokens source (Info i o) = do
    let opSequence = Op (FromStream source) (formStreamOpSequence tokens i)
    (dest, vals) <- processStreamOpSequence opSequence (Info i o)
    vals <- vals
    o <- pushValuesToStream vals o dest
    intakeToken (skipToNextLine tokens) (Info i o)

formStreamOpSequence :: [Token] -> [[Int]] -> StreamOpSequence
formStreamOpSequence ((SendOp):t) i = Op Send (formStreamOpSequence t i)
formStreamOpSequence ((ShowOp):t) i = Op Print (formStreamOpSequence t i)
formStreamOpSequence ((OutputStream):(DigitLit n):t) i = End (ToEndStream n)
formStreamOpSequence ((BClose):t) i = End UndefinedEnd
formStreamOpSequence ((CombineOp):(BOpen):t) i = (Combined opSequence (formStreamOpSequence tokens i)) where
                                            opSequence = formStreamOpSequence t i
                                            tokens = skipToEndBracket t
formStreamOpSequence ((AddOp):t) i = Op Add (formStreamOpSequence t i)
formStreamOpSequence ((SubOp):t) i = Op Sub (formStreamOpSequence t i)
formStreamOpSequence ((MultiplyOp):(DigitLit n):t) i = Op (Multiply n) (formStreamOpSequence t i)
formStreamOpSequence ((DivideOp):(DigitLit n):t) i = Op (Divide n) (formStreamOpSequence t i)
formStreamOpSequence ((NegateOp):t) i = Op Negate (formStreamOpSequence t i)
formStreamOpSequence ((ModOp):t) i = Op Mod (formStreamOpSequence t i)
formStreamOpSequence ((PowOp):t) i = Op Pow (formStreamOpSequence t i)
formStreamOpSequence ((MultiplyOp):t) i = Op CombinedMultiply (formStreamOpSequence t i)
formStreamOpSequence ((DivideOp):t) i = Op CombinedDivide (formStreamOpSequence t i)
formStreamOpSequence ((OnlyOp):(DigitLit n):t) i = Op (Only n) (formStreamOpSequence t i)
formStreamOpSequence ((SkipOp):(DigitLit n):t) i = Op (Skip n) (formStreamOpSequence t i)
formStreamOpSequence ((InputStream):(DigitLit streamVal):t) i = Op (FromStream (i!!streamVal)) (formStreamOpSequence t i)
formStreamOpSequence ((LastOp):(DigitLit n):t) i = Op (Last n) (formStreamOpSequence t i)
formStreamOpSequence ((FromLastOp):(DigitLit sn):(DigitLit n):t) i = Op (FromLast sn n) (formStreamOpSequence t i)
formStreamOpSequence ((GenOp):(DigitLit n):t) i = Op (Gen n) (formStreamOpSequence t i)
formStreamOpSequence a i = End UndefinedEnd

-- Stream Processing -------------------------------------------------------------------------------------
streamDestination :: StreamOpSequence -> Int
streamDestination (Combined seq1 seq2) = streamDestination seq2
streamDestination (Op op seq) = streamDestination seq
streamDestination (End (ToEndStream n)) = n

processStreamOpSequence :: StreamOpSequence -> StreamInfo -> IO (Int, IO [Int])
processStreamOpSequence (Op (FromStream n) (Op (Skip s) seq)) info = do
    let (_, skipped) = splitAt s n
    return (streamDestination seq, recursiveProcess seq info skipped s)
processStreamOpSequence (Op (FromStream n) seq) info = return (streamDestination seq, recursiveProcess seq info n 0)

recursiveProcess :: StreamOpSequence -> StreamInfo -> [Int] -> Int -> IO [Int]
recursiveProcess seq info (val:inStream) count = do
    (processedVal, info) <- calculateStreamOpSequence seq info val count
    processedVal <- processedVal
    if processedVal == 9223372036854775807 then --max int indicates error/abort
        return []
    else do
        nextVal <- recursiveProcess seq info inStream (count+1)
        return (processedVal:nextVal)
recursiveProcess seq info [] count = return []

calculateStreamOpSequence :: StreamOpSequence -> StreamInfo -> Int -> Int -> IO (IO Int, StreamInfo)
calculateStreamOpSequence (Op Send seq) info val count = calculateStreamOpSequence seq info val count
calculateStreamOpSequence (Op (Only n) seq) info val count
    | n > count = calculateStreamOpSequence seq info val count
    | otherwise = return (return 9223372036854775807, info)
calculateStreamOpSequence (End (ToEndStream n)) (Info i o) val count = do
    o <- pushValuesToStream [val] o n
    return (return val, (Info i o))
calculateStreamOpSequence (End UndefinedEnd) info val count = return (return val, info)
calculateStreamOpSequence (Combined seq1 seq2) info val count = processCombinedOp seq1 seq2 info val count
calculateStreamOpSequence (Op (Multiply n) seq) info val count = calculateStreamOpSequence seq info (val*n) count
calculateStreamOpSequence (Op (Divide n) seq) info val count = calculateStreamOpSequence seq info (quot val n) count
calculateStreamOpSequence (Op Negate seq) info val count = calculateStreamOpSequence seq info (-val) count
calculateStreamOpSequence (Op Print seq) info val count = do putStr ((show val) ++ " "); calculateStreamOpSequence seq info val count
calculateStreamOpSequence op info val count = do
    putStrLn ("Unknown op" ++ (show op))
    return (return val, info)

processCombinedOp :: StreamOpSequence -> StreamOpSequence -> StreamInfo -> Int -> Int -> IO (IO Int, StreamInfo)
processCombinedOp (Op (FromStream s1) seq1) (Op Send seq2) info val count = do
    (seq1Val, info) <- calculateStreamOpSequence seq1 info (s1!!count) count
    seq1Val <- seq1Val
    calculateCombinedOp seq2 info seq1Val val count
processCombinedOp (Op (Gen n) seq1) (Op Send seq2) info val count = do
    (seq1Val, info) <- calculateStreamOpSequence seq1 info n count
    seq1Val <- seq1Val
    calculateCombinedOp seq2 info seq1Val val count
processCombinedOp (Op (Last n) seq1) (Op Send seq2) (Info i o) val count = do
    (seq1Val, info) <- calculateStreamOpSequence seq1 (Info i o) (last (o!!n)) count
    seq1Val <- seq1Val
    calculateCombinedOp seq2 info seq1Val val count
processCombinedOp (Op (FromLast sn n) seq1) (Op Send seq2) (Info i o) val count = do
    let stream = (o!!n)
    let (_, value:_) = splitAt ((length stream)-(sn+1)) stream
    (seq1Val, info) <- calculateStreamOpSequence seq1 (Info i o) (value) count
    seq1Val <- seq1Val
    calculateCombinedOp seq2 info seq1Val val count

calculateCombinedOp :: StreamOpSequence -> StreamInfo -> Int -> Int -> Int -> IO (IO Int, StreamInfo)
calculateCombinedOp (Op Add seq) info val2 val1 count = calculateStreamOpSequence seq info (val1+val2) count
calculateCombinedOp (Op Sub seq) info val2 val1 count = calculateStreamOpSequence seq info (val1-val2) count
calculateCombinedOp (Op CombinedMultiply seq) info val2 val1 count = calculateStreamOpSequence seq info (val1*val2) count
calculateCombinedOp (Op CombinedDivide seq) info val2 val1 count = calculateStreamOpSequence seq info (quot val1 val2) count
calculateCombinedOp (Op Mod seq) info val2 val1 count = calculateStreamOpSequence seq info (val1 `mod` val2) count
calculateCombinedOp (Op Pow seq) info val2 val1 count = calculateStreamOpSequence seq info (val1^val2) count

pushValuesToStream :: [Int] -> [[Int]] -> Int -> IO [[Int]]
pushValuesToStream values streams streamIndex = do
    let (start, stream:end) = splitAt streamIndex streams
    let newstream = stream ++ values
    return (start ++ (newstream:end))

produceOutput :: StreamInfo -> Int -> IO StreamInfo
produceOutput (Info i o) numberofstreams = do
    let (trimmed, _) = splitAt numberofstreams o
    mapM_ (lineOutput) (transpose trimmed)
    return (Info i o)

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

    outputStreams <- intakeToken tokens (Info inputStreams outputStreams)
    return ()

