{
module Parser where 

import Lexer as L 
import Language  
}

%name calc 
%tokentype {Token}
%error {parseError }

%token
   '.' {Point} 
   "->" { Arrow}
   ',' { Comma}
   '=' { Assign} 
   fun { Lambda} 
   let  { Let} 
   in { In} 
   letrec { Letrec} 
   case { Case} 
   of { Of}
   '(' { LParent} 
   ')' { RParent}
   ';' { SemiColon} 
   Pack { Pack}
   '{' { LBrace}
   '}' { RBrace}
   num { Num $$} 
   ident { Ident $$ }  
   '+' { Plus}
   '-' { Minus} 
   '*' { Mul} 
   '/' { Div} 
   '<' { L.LT} 
   '<=' { L.LE} 
   '==' { Eq}
   '/=' { NEQ}
   '>' { L.GT} 
   '>=' { L.GE} 
   and { And} 
   or { Or} 

%right in 
%left '+' '-'
%left '*' '/'
%nonassoc '|' '&'

%%

Program :: { CoreProgram }
Program 
      : ScList                    { mkProgram $1 }
      | {- empty -}               { [] }  

Sc :: {(Name, [Name], CoreExpr)}
Sc : Var VarList '=' Expr            { mkSc $1 $2 $4 }           

Expr :: {CoreExpr }
Expr : Expr AExpr                   { app $1 $2 }
      | BinOp                       { $1 }
      | let Defns in Expr           { let_ False $2 $4}
      | letrec Defns in Expr        { let_ True  $2 $4 }      
      | case Expr of Alts           { case_ $2 $4 }
      | fun VarList "->" Expr       { fun $2 $4 }
      | AExpr                       { $1 } 

AExpr ::{ CoreExpr }
AExpr : Var                         { var $1 }
      | num                         { num $1}    
      | Pack '{' num ',' num '}'    { constr $3 $5 }
      | '(' Expr ')'                { $2 }

----------------- Defns ----------------------------

Defns : RawDefns                    { reverse $1 }

RawDefns :: {[(Name, CoreExpr)]}
RawDefns : Defns ';' Defn              { $3 : $1 }
      | Defn                        { (: []) $1 }     

Defn :: {(Name, CoreExpr)}
Defn : Var '=' Expr                 { (,) $1 $3 }      

----------------- Alts ------------------------
Alts : RawAlts                      { reverse $1 }


RawAlts :: {[] CoreAlt}
RawAlts : Alts ';' Alt                 { $3 : $1 }
      | Alt                         { (:[]) $1}


Alt :: {CoreAlt} 
Alt : '<' num '>' VarList "->" Expr { alt $2 $4 $6 } 

------------------- Bin Op --------------------------

BinOp :: {CoreExpr}
BinOp : Expr '+' Expr                 { mkBinOp "+" $1 $3 }
        | Expr '-' Expr               { mkBinOp "-" $1 $3 }
        | Expr '*' Expr               { mkBinOp "*" $1 $3 }
        | Expr '/' Expr               { mkBinOp "/" $1 $3 }      
        | Expr '<' Expr               { mkBinOp "<" $1 $3 }
        | Expr '<=' Expr              { mkBinOp "<=" $1 $3 }
        | Expr '>=' Expr              { mkBinOp ">=" $1 $3 }
        | Expr '>' Expr               { mkBinOp ">" $1 $3 }
        | Expr '==' Expr              { mkBinOp "==" $1 $3 }
        | Expr '/=' Expr              { mkBinOp "/=" $1 $3 }
        | Expr and Expr               { mkBinOp "&" $1 $3 }      
        | Expr or Expr                { mkBinOp "|" $1 $3 }

--------- helpers ----------------------------
Var :: {Name}
Var : ident                      { $1 }   

ScList :: {[CoreScDefn]}
ScList : RawScList                { reverse $1 }

RawScList :: { [] CoreScDefn }
RawScList 
      : ScList ';' Sc            { $3 : $1 }
      | Sc                       { (:[]) $1 }

VarList :: {[Name]}
VarList : RawVarList              { reverse $1 }

RawVarList :: {[Name]}
RawVarList 
      : VarList Var                 { $2 : $1 }
      | {- empty -}               { [] }  

---------------------------------------------------

{
parseError :: [Token] -> a 
parseError _ = error "Parse error"
}