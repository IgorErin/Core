{
module Parser where 

import qualified Lexer as L 
import Language  
}

%name calc 
%tokentype {L.Token}
%error {parseError }

%token
   '.' { L.Point} 
   "->" { L.Arrow}
   ',' { L.Comma}
   '=' { L.Assign} 
   fun { L.Lambda} 
   let  { L.Let} 
   in { L.In} 
   letrec { L.Letrec} 
   case { L.Case} 
   of { L.Of}
   '(' { L.LParent} 
   ')' { L.RParent}
   ';' { L.SemiColon} 
   Pack { L.Pack}
   '{' { L.LBrace}
   '}' { L.RBrace}
   num { L.Num $$} 
   ident { L.Ident $$ }  
   '+' { L.Plus}
   '-' { L.Minus} 
   '*' { L.Mul} 
   '/' { L.Div} 
   '<' { L.LT} 
   '<=' { L.LE} 
   '==' { L.Eq}
   '/=' { L.NEQ}
   '>' { L.GT} 
   '>=' { L.GE} 
   and { L.And} 
   or { L.Or } 

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
      : RawScList ';' Sc            { $3 : $1 }
      | Sc                       { (:[]) $1 }

VarList :: {[Name]}
VarList : RawVarList              { reverse $1 }

RawVarList :: {[Name]}
RawVarList 
      : RawVarList Var                 { $2 : $1 }
      | {- empty -}               { [] }  

---------------------------------------------------

{
parseError :: [L.Token] -> a 
parseError _ = error "Parse error"
}