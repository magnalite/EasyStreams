{
module Tokens where
}

%wrapper "basic"

$letter = [a-zA-Z]
$digit = 0-9
$stringContent = [~"~\;]

tokens :-
    $white+ ;
    \#.+\# ;

    $digit+ {\s -> DigitLit (read s)} 
    \"$stringContent+\" {\s -> StringToken s}
    \; {\s -> LineEnd}
    \-> {\s -> SendOp}

    InStream {\s -> InputStream}
    OutStream {\s -> OutputStream}
    LoadStreams {\s -> LoadOp}
    OutputStreams {\s -> OutputOp}
    Show {\s -> ShowOp}
    Copy {\s -> CopyOp}
{
data Token =
    StringToken String |
    LineEnd |
    SendOp |
    InputStream |
    OutputStream |
    DigitLit Int |
    LoadOp |
    OutputOp |
    ShowOp |
    CopyOp
    deriving (Eq,Show)
}