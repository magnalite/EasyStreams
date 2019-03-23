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

    $digit+ {\s -> DigitLit} 
    \"$stringContent+\" {\s -> String}
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
    Ref |
    String |
    LineEnd |
    SendOp |
    InputStream |
    OutputStream |
    DigitLit |
    LoadOp |
    OutputOp |
    ShowOp |
    CopyOp
    deriving (Eq,Show)
}