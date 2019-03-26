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
    >> {\s -> CombineOp}
    \( {\s -> BOpen}
    \) {\s -> BClose}

    InStream {\s -> InputStream}
    OutStream {\s -> OutputStream}
    LoadStreams {\s -> LoadOp}
    OutputStreams {\s -> OutputOp}
    Show {\s -> ShowOp}
    Gen {\s -> GenOp}
    Add {\s -> AddOp}
    Multiply {\s -> MultiplyOp}
    Last {\s -> LastOp}
    Only {\s -> OnlyOp}
{
data Token =
    StringToken String |
    LineEnd |
    SendOp |
    CombineOp |
    BOpen |
    BClose |
    InputStream |
    OutputStream |
    DigitLit Int |
    LoadOp |
    OutputOp |
    ShowOp |
    GenOp |
    AddOp |
    MultiplyOp |
    LastOp |
    OnlyOp
    deriving (Eq,Show)
}