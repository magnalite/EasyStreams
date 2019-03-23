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
    \; ;
    
    InStream$digit+     {\s -> TokenInStream}
    OutStream$digit+    {\s -> TokenOutStream}
    \->                 {\s -> TokenSend}
    $digit+             {\s -> TokenInt}
    \"$stringContent+\" {\s -> TokenString}
    $letter+            {\s -> TokenOp}

{
data Token =
    TokenInStream |
    TokenOutStream | 
    TokenSend |
    TokenInt |
    TokenString |
    TokenOp 
    deriving (Eq,Show)
}