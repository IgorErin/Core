module Show where 

import qualified State as S 
import qualified PSeq as P
import qualified Language as L
import qualified Heap as H
------------------------ Program ---------------------------

showStates :: [S.TiState] -> P.T
showStates states = P.interleav P.nl $ map showState states

showState :: S.TiState -> P.T  
showState (stack, _, heap, _, _) = 
     P.interleav P.nl [
             P.str "======== Stack =========", 
             showStack stack heap,
             P.str "********* Heap **********", 
             showHeap heap ]

showStack :: S.TiStack -> S.TiHeap -> P.T 
showStack stack heap = 
    P.interleav sep $ map showItem $ reverse stack
    where 
        showItem addr =
            P.merge [
                P.str "addr: ",showAddr addr, P.nl,
                showNode (H.hLookup heap addr) heap  
            ]
        sep = P.merge [ P.nl, P.str "------ Frame ------", P.nl ]

showAddr :: H.Addr -> P.T 
showAddr a = P.merge [ P.str "(addr: ", P.str $ show a, P.str ")" ]  


showStat :: S.TiStat -> P.T 
showStat = P.str . show 

showHeap :: S.TiHeap -> P.T 
showHeap heap = 
    P.interleav P.nl . 
    map (\(key, value) -> P.merge [P.str $ show key, P.str " -> ", showNode  value heap ]) .
    (\(_, _, h) -> h) $
    heap
    
showNode :: S.Node -> S.TiHeap -> P.T
showNode (S.NNum n) _ =  P.merge [P.str "NNum ", P.str $ show n]
showNode (S.NApp f x) heap = P.merge [ P.str "NApp ", showNode nf heap , P.str " (", showNode nx heap , P.str ")"  ]
    where 
        nf = H.hLookup heap f 
        nx = H.hLookup heap x

showNode (S.NSupercomb name _ _ ) _ = P.merge [P.str name]

----------------------- Language --------------------------------

pprExpr :: L.CoreExpr -> P.T
pprExpr (L.EConstr f s) = P.merge [ P.str "<", P.str $ show f, P.str ", ", P.str $ show s, P.str ">" ]
pprExpr (L.ENum n) = P.str $ show n 
pprExpr (L.EVar v) = P.str v 
pprExpr (L.EAp x y) = pprExpr x `sep` pprExpr y
    where 
        ws = P.str " " 
        sep f s = f `P.append` ws `P.append` s 
pprExpr (L.ELet isrec ls body) = 
    P.merge [
        P.str recflag, P.nl,
        P.ws, P.indent $ pprDefns ls, P.nl,
        P.str "in ", pprExpr body
    ]
    where recflag = if isrec then "letrec" else "let"
pprExpr (L.ELam vars body) = 
    P.merge [ 
        P.str "lambda (",
        P.interleav sep $ map P.str vars ,
        P.str ") ",
        P.indent $ pprExpr body
    ]
    where sep = P.str ", "
pprExpr (L.ECase scr vars) = 
    P.merge [ 
        P.str "match ", pprExpr scr, P.str "with", P.nl,
        P.interleav (P.str "|") $ map pvar vars 
    ]
    where
        pvar :: L.CoreAlt -> P.T 
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

pprDefns :: [(L.Name, L.CoreExpr)] -> P.T
pprDefns defns = 
    let sep = P.merge [P.str ";", P.nl ] in 
    P.interleav sep $ map pprDefn defns

pprDefn :: (L.Name, L.CoreExpr) -> P.T 
pprDefn (name, expr) = P.merge [P.str name, P.str " = ", P.indent $ pprExpr expr ]

