module Language where 

import qualified PSeq as P

--------------- Types -----------------

type Name = String 
type IsRec = Bool 

type Alt a = (Int, [a], Expr a)
type CoreAlt = Alt Name   

type ScDefn a = (Name, [a], Expr a)
type CoreScDefn = ScDefn Name

type Program a = [] (ScDefn a)
type CoreProgram  = Program Name 

data Expr a = 
    EVar Name
    | ENum Int 
    | EConstr Int Int 
    | EAp (Expr a) (Expr a)
    | ELet IsRec [(a, Expr a)] (Expr a)
    | ECase (Expr a) [Alt a]
    | ELam [a] (Expr a)
    deriving (Show, Eq)  

type CoreExpr = Expr Name 

------------------ Helpers -----------------

var :: Name -> Expr a
var = EVar 
num :: Int -> Expr a
num = ENum 
constr :: Int -> Int -> Expr a
constr = EConstr 
app :: Expr a -> Expr a -> Expr a
app = EAp 
let_ :: IsRec -> [(a, Expr a)] -> Expr a -> Expr a
let_ = ELet 
case_ :: Expr a -> [Alt a] -> Expr a
case_ = ECase 
fun :: [a] -> Expr a -> Expr a
fun = ELam 

alt :: Int -> [Name] -> CoreExpr -> CoreAlt 
alt = (,,)

mkBinOp :: Name -> CoreExpr -> CoreExpr -> CoreExpr  
mkBinOp name f = EAp (EAp (EVar name) f) 

mkSc :: Name -> [Name] -> CoreExpr -> CoreScDefn  
mkSc = (,,)

mkProgram :: [ScDefn Name] -> CoreProgram  
mkProgram x = x  

----------------- Stdlib ------------------------

-- I x = x ;
-- K x y = x ;
-- K1 x y = y ;
-- S f g x = f x (g x) ;
-- compose f g x = f (g x) ;
-- twice f = compose f f

predef :: CoreProgram 
predef = [
    ("I", ["x"], EVar "x"),
    ("K", ["x", "y"], EVar "x"),
    ("K1", ["x", "y"], EVar "y"),
    ("S", ["f", "g", "x"], EAp (EAp (EVar"f") (EVar "x")) (EAp (EVar"g") (EVar "x"))),
    ("compose", ["f", "g", "x"], EAp (EVar"f") (EAp (EVar"g") (EVar "x"))),
    ("twice", ["f"], EAp (EAp (EVar "compose") (EVar "f")) (EVar "f"))
    ] 

------------------- Print -------------------------

pprExpr :: CoreExpr -> P.T
pprExpr (EConstr f s) = P.merge [ P.str "<", P.str $ show f, P.str ", ", P.str $ show s, P.str ">" ]
pprExpr (ENum n) = P.str $ show n 
pprExpr (EVar v) = P.str v 
pprExpr (EAp x y) = pprExpr x `sep` pprExpr y
    where
        ws = P.str " " 
        sep f s = f `P.append` ws `P.append` s 
pprExpr (ELet isrec ls body) = 
    P.merge [
        P.str recflag, P.nl,
        P.ws, P.indent $ pprDefns ls, P.nl,
        P.str "in ", pprExpr body
    ]
    where recflag = if isrec then "letrec" else "let"
pprExpr (ELam vars body) = 
    P.merge [ 
        P.str "lambda (",
        P.interleav sep $ map P.str vars ,
        P.str ") ",
        P.indent $ pprExpr body
    ]
    where sep = P.str ", "
pprExpr (ECase scr vars) = 
    P.merge [ 
        P.str "match ", pprExpr scr, P.str "with", P.nl,
        P.interleav (P.str "|") $ map pvar vars 
    ]
    where
        pvar :: CoreAlt -> P.T 
        pvar (number, vs, body) =
            let 
                sep = P.str ", " 
                pvars vars' = P.interleav sep $ map P.str vars' 
            in 
            P.merge [ 
                P.str "C<", 
                P.interleav sep 
                    [
                         P.str $ show number, 
                         pvars vs
                    ],
                P.str ">",
                P.str " -> ",
                pprExpr body 
            ]

pprDefns :: [(Name, CoreExpr)] -> P.T
pprDefns defns = 
    let sep = P.merge [P.str ";", P.nl ] in 
    P.interleav sep $ map pprDefn defns

pprDefn :: (Name, CoreExpr) -> P.T 
pprDefn (name, expr) = P.merge [P.str name, P.str " = ", P.indent $ pprExpr expr ]
