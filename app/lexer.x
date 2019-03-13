{
    module Lexer 
    ( alexScanTokens
    ) where
}

%wrapper "basic"

$digit = 0-9
$lower = [a-z]
$upper = [A-Z]
$alpha = [a-zA-Z]

tokens :-

    $white+                         ;
    \#.*                            ;
    var                             { \s -> Var}
    let                             { \s -> Let}
    in                              { \s -> In}
    match                           { \s -> Match}
    case                            { \s -> Case}
    type                            { \s -> Type}
    otherwise                       { \s -> Otherwise}
    true                            { \s -> TrueT}
    false                           { \s -> FalseT}
    \=\>                            { \s -> Follows}
    $digit+                         { \s -> Int (read s)}
    $digit+\.$digit+                { \s -> Float (read s)}
    \'.\'                           { \s -> Char (head (tail s)) }
    $upper [$alpha $digit \_ \']*   { \s -> TypeId s}
    $lower [$alpha $digit \_ \']*   { \s -> VarId s}
    \|                              { \s -> Guard}
    \=                              { \s -> Declare}
    \{                              { \s -> CurlyOpen}
    \}                              { \s -> CurlyClose}
    \:\:                            { \s -> Annotate}
    \[                              { \s -> SquareOpen}
    \]                              { \s -> SquareClose}
    \(                              { \s -> ParenOpen}
    \)                              { \s -> ParenClose}
    \-\>                            { \s -> Entails}
    \,                              { \s -> Comma}
    \+                              { \s -> LevelOneOp s} 
    \-                              { \s -> LevelOneOp s}
    \+\+                            { \s -> LevelOneOp s}
    \&\&                            { \s -> LevelOneOp s}
    \|\|                            { \s -> LevelOneOp s}
    \*                              { \s -> LevelTwoOp s}
    \/                              { \s -> LevelTwoOp s}
    \%                              { \s -> LevelTwoOp s}
    \=\=                            { \s -> LevelTwoOp s}
    \!\=                            { \s -> LevelTwoOp s}
    \<                              { \s -> LevelThreeOp s} 
    \>                              { \s -> LevelThreeOp s}
    \<\=                            { \s -> LevelThreeOp s}
    \>\=                            { \s -> LevelThreeOp s}
    \!                              { \s -> LevelFourOp s}
    \~                              { \s -> LevelFourOp s}
    \:                              { \s -> LevelFourOp s}

{
data Token = 
        Var                 |
        Let                 |
        In                  |
        Match               |
        Case                |
        Type                |
        Otherwise           |
        TrueT               |
        FalseT              |
        Follows             |
        Int Int             |
        Float Float         |
        Char Char           |
        TypeId String       |
        VarId String        |
        Guard               |
        Declare             |
        CurlyOpen           |
        CurlyClose          |
        Annotate            |
        SquareOpen          |
        SquareClose         |
        ParenOpen           |
        ParenClose          |
        Entails             |
        Comma               |
        LevelOneOp String   |
        LevelTwoOp String   |
        LevelThreeOp String |
        LevelFourOp String
        deriving (Eq, Show)
}