{
module Lexer where
}

%wrapper "basic"

$digit = 0-9            -- digits
$alpha = [a-zA-Z]       -- alphabetic characters

@ident = $alpha [$alpha $digit]*
@number = digit+

tokens :-
  $white+               ;
  "--".*                ;
  "let"                 { \_ -> Let }
  "in"                  { \_ -> In }
  "->"                  { \_ -> Arrow } 
  $digit+               { \s -> Num (read s) }
  "=="                  { \_ -> Lexer.Eq }
  "<>"                  { \_ -> Lexer.NEQ}
  "="                   {\_ -> Assign}
  "fun"                  {\_ -> Lambda }
  "<="                  {\_ -> LE }
  "<"                   {\_ -> Lexer.LT}
  ">="                  {\_ -> Lexer.GE }
  ">"                   {\_ -> Lexer.GT }
  "letrec"              {\_ -> Letrec }
  "let"                 {\_ -> Let }
  "*"                   {\_ -> Mul }
  "+"                   {\_ -> Plus }
  "-"                   { \_ -> Minus}
  "&"                   { \_ -> And}
  "|"                   { \_ -> Or }
  "/"                   {\_ -> Div }
  "("                   {\_ -> LParent }
  ")"                   { \_ -> RParent}
  "{"                   { \_ -> LBrace }
  "}"                   { \_ -> RBrace }
  "case"                {\_ -> Case }
  "of"                  {\_ -> Of }    
  ";"                   {\_ -> SemiColon}
  "."                   {\_ -> Point }
  ","                   {\_ -> Comma }
  "Pack"                {\_ -> Pack }
  @ident                 {\s -> Ident s}

{
-- Each action has type :: String -> Token

-- The token type:
data Token
  = 
  -- key words
  Point
  | Arrow
  | Comma
  | Assign 
  | Lambda 
  | Let 
  | In 
  | Letrec 
  | Case 
  | Of
  | LParent 
  | RParent
  | SemiColon 
  | Pack
  -- { }
  | LBrace
  | RBrace
-- Values
  | Num Int
  | Ident String 
-- binop 
  | Plus
  | Minus 
  | Mul 
  | Div 
  | LT 
  | LE 
  | Eq
  | NEQ
  | GT 
  | GE 
  | And 
  | Or 
  deriving (Eq, Show)
}