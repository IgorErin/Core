module Mark1 where 

import qualified Language as L
import qualified Utils as U 
import qualified Data.List as List (head) 
import qualified Data.Map as Map
import qualified PSeq as P

compile :: L.CoreProgram -> U.TiState  
compile program = 
    (initStack, U.initDump, initHeap, globals, U.tiStatInit)
        where
            scDefs = program ++ L.predef

            (globals, initHeap) = buildHeap scDefs

            initStack = [mainAddr]
            mainAddr = U.gLook "main" globals 

            buildHeap = foldl foldStep (U.gInit, U.hInit )  
                where 
                foldStep (g ,h) (name, args, body) =  
                    let (newHeap, addr) = U.hAlloc (U.NCombinator name args body) h in 
                    let newGlobals = U.gInsert name addr g in 
                    (newGlobals, newHeap)

final :: U.TiState -> Bool 
final (stack, _, heap, _, _) = 
    case stack of 
        [] -> error "Empty stack" 
        [node] -> U.isDataNode (U.hLook node heap)
        _ -> False

glue :: U.TiState -> U.TiState 
glue = id 

--------------------- Steps -----------------------------------

numStep :: U.TiState -> Int -> U.TiState
numStep = error "Number applied as function"

appStep :: U.TiState -> U.Addr -> U.Addr -> U.TiState 
appStep (stack, dump, heap, globals, stat) a1 _ = (a1 : stack, dump, heap, globals, stat)

instanciate :: L.CoreExpr -> U.TiHeap -> U.TiGlobals -> (U.TiHeap, U.Addr)
instanciate (L.ENum n) heap _ = U.hAlloc (U.NNum n) heap 
instanciate (L.EAp f arg) heap globals = 
    U.hAlloc (U.NApp a1 a2) heap2
    where 
        (heap1, a1) = instanciate f heap globals 
        (heap2, a2) = instanciate arg heap1 globals
instanciate (L.EVar v) heap globals = (heap, varAddr)
    where varAddr = U.gLook v globals 
instanciate _ _ _= undefined            

combStep :: U.TiState -> L.Name -> [L.Name] -> L.CoreExpr -> U.TiState 
combStep (stack, dump, heap, globals, stat) _ pars body =
    (newStack, dump, newHeap, globals, stat)

    where 
        newStack = newAddr : drop (length pars + 1) stack 

        getArgs :: U.TiStack -> [] U.Addr 
        getArgs =  map getArg
            where 
            getArg addr = 
                case U.hLook addr heap of 
                U.NApp _ arg -> arg
                _ -> error "Not a app in arg lookup"

        newGlobals = Map.union globals $ Map.fromList $ zip pars $ getArgs (drop 1 stack) 

        (newHeap, newAddr) = instanciate body heap newGlobals

step :: U.TiState -> U.TiState 
step state = dispatch (U.hLook (List.head stack) heap)
    where   
        (stack, _, heap, _, _) = state 

        dispatch (U.NNum n) = numStep state n 
        dispatch (U.NApp f a) = appStep state f a
        dispatch (U.NCombinator name args body) = combStep state name args body         

--------------------- Eval -----------------------------------

start :: U.TiState -> [U.TiState]
start state = state : restStates 
    where 
        restStates  
            | final state = []
            | otherwise = start $ glue $ step state  

getResult :: [U.TiState] -> Int
getResult ls = n
    where 
        n = 
            case U.hLook (head stack) heap of 
                U.NNum num -> num 
                _ -> error "Number expected after execution"
        
        (stack, _, heap, _, _) = last ls

run :: L.CoreProgram -> Int
run program = result
    where 
        compiled = compile program 
        states = start compiled 
        result = getResult states

------------------------ Show ---------------------------

showResult :: [U.TiState] -> String
showResult states = P.display $ P.interleav P.nl $ map showState states

showState :: U.TiState -> P.T  
showState (stack, _, heap, _, _) = 
     P.interleav P.nl [
             P.str "======== Stack =========", 
             showStack stack heap
            --  , 
            --  P.str "------- Stat -----",
            --  showStat stat 
            ]

showStack :: U.TiStack -> U.TiHeap -> P.T 
showStack stack heap = 
    P.interleav sep $ map showItem $ reverse stack
    where 
        showItem addr =
            P.merge [
                P.str "addr: ",showAddr addr, P.nl,
                showNode (U.hLook addr heap) heap  
            ]
        sep = P.merge [ P.nl, P.str "------ Frame ------", P.nl ]

showAddr :: U.Addr -> P.T 
showAddr a = P.merge [ P.str "(addr: ", P.str $ show a, P.str ")" ]  

showNode :: U.Node -> U.TiHeap -> P.T 
showNode (U.NApp f a ) heap =
    P.interleav (P.str " ") [ 
        P.str "APP", showAddr f, P.str " ",
        P.str "(", 
        showNode (U.hLook a heap) heap,
        P.str ")" ] 
showNode (U.NNum n) _ = P.merge [ P.str "NUM: ", P.str $ show n ]
showNode (U.NCombinator name args body) _ = 
    P.merge [ 
        P.str "COMBINATOR ", P.str name, P.str " (", pArgs, P.str ") =", 
        P.str " ", P.indent $ L.pprExpr body 
    ]

    where 
        pArgs = P.interleav (P.str " ") $ map P.str args 

showStat :: U.TiStat -> P.T 
showStat = P.str . show 
            