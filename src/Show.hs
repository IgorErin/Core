module Show where 

import qualified State as S 
import qualified PSeq as P
import qualified Language as L
import qualified Heap as H
import qualified Language as S

------------------------ Program ---------------------------

strStates :: [S.TiState] -> String
strStates = P.display . showStates 

showStates :: [S.TiState] -> P.T
showStates states = P.interleav P.nl $ map showState states

showState :: S.TiState -> P.T  
showState (stack, dump, heap, _, stat) = 
     P.interleav P.nl [
            --  P.str "@@@@@@@@@@ Stat @@@@@@@@@@@",
            --  showStat stat,
             P.str "======== Stack =========", 
             showStack stack heap,
             P.str "&&&&&&&&& DUMP &&&&&&&&&&", 
             showDump dump heap 
            --  P.str "********* Heap **********", 
            --  showHeap heap
              ]

showDump :: [S.TiStack] -> S.TiHeap -> P.T 
showDump stacks heap  = 
    let sep = P.merge [P.nl, P.str "---------  in Dump ------------", P.nl ]
    in P.interleav sep $ map (`showStack` heap) stacks 

showStat :: S.TiStat -> P.T
showStat stat = P.str $ "Count: " ++ show stat 

strStack :: S.TiStack -> S.TiHeap -> String
strStack stack heap = P.display $ showStack stack heap

showStack :: S.TiStack -> S.TiHeap -> P.T 
showStack stack heap = P.interleav sep $ map showItem $ reverse stack
    where 
        showItem addr =
            P.merge [
                P.str "addr: ",showAddr addr, P.nl,
                showNode (H.hLookup heap addr) heap  
            ]
        sep = P.merge [ P.nl, P.str "------ Frame ------", P.nl ]

showAddr :: H.Addr -> P.T 
showAddr a = P.merge [ P.str "(addr: ", P.str $ show a, P.str ")" ]  

showHeap :: S.TiHeap -> P.T 
showHeap heap@(info, _, mapping) = 
    let content = 
            P.interleav P.nl . map (\(key, value) -> P.merge [P.str $ show key, P.str " -> ", showNode value heap ]) $ mapping
    in P.merge
     [ P.str "Alloc count: ", P.str $ show $ H.heapAllocCount info, P.nl,
       P.str "Realloc count: ", P.str $ show $ H.heapReallocCount info, P.nl,
       content ]
    
showNode :: S.Node -> S.TiHeap -> P.T
showNode  = showNodeRec []

onlyName :: S.TiHeap ->  S.Node -> P.T
onlyName heap (S.NApp l r) =
    let left = H.hLookup heap l 
        right = H.hLookup heap r 
    in P.interleav (P.str " ") [P.str "App", onlyName heap left, P.str "(", onlyName heap right, P.str ")"]
onlyName _ (S.NSupercomb name _ _) = P.str name
onlyName _ (S.NNum num) = P.str $ show num 
onlyName heap (S.NInd addr) = onlyName heap $ H.hLookup heap addr 
onlyName _ (S.NPrim name _) = P.str name 
onlyName _ (S.NData tag _) = P.str $ "tag :: " ++ show tag 

recCheck :: S.TiHeap -> [H.Addr] -> H.Addr -> P.T
recCheck heap adds add = 
    if add `elem` adds 
    then onlyName heap $ H.hLookup heap add
    else showNodeRec adds (H.hLookup heap add) heap 

showNodeRec  :: [H.Addr] -> S.Node -> S.TiHeap -> P.T
showNodeRec _ (S.NNum n) _ =  P.merge [P.str "NNum ", P.str $ show n]
showNodeRec adds (S.NApp f x) heap = 
    let adds' = f : x : adds  
        recCheck' = recCheck heap adds'

        left = recCheck' f 
        right = recCheck' x 
    in P.merge [ P.str "NApp ", left , P.str " (", right , P.str ")"  ]
showNodeRec  _ (S.NSupercomb name _ _ ) _ = P.merge [P.str name]
showNodeRec adds (S.NInd n) heap = 
    P.merge [P.str "NInd ", showNodeRec adds (H.hLookup heap n) heap ]
showNodeRec _ (S.NPrim name _) _ = P.str name
showNodeRec adds (S.NData tag dataArgs) heap = 
    let adds' = dataArgs ++ adds 
        recCheck' = recCheck heap adds'

        args = map recCheck' dataArgs 
    in P.merge [
        P.str $ "Data " ++ show tag ++ " (" ,
        P.interleav (P.str ", ") args,
        P.str ")" 
        ]

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

