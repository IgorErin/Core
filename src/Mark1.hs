module Mark1 where 

import qualified Language as L
import qualified State as S 
import qualified PSeq as P
import qualified Heap as H

import qualified Data.List as List (head) 

compile :: L.CoreProgram -> S.TiState  
compile program = 
    (initStack, S.initTiDump, initHeap, globals, S.tiStatInit)
        where
            scDefs = L.predef ++ program

            (globals, initHeap) = buildHeap scDefs

            initStack = [mainAddr]
            mainAddr = S.gLookup globals "main"

            buildHeap = foldl foldStep (S.gInit, H.hInit )  
                where 
                foldStep (g ,h) (name, args, body) =  
                    let (newHeap, addr) = H.hAlloc h (S.NSumpercomb name args body) in 
                    let newGlobals = S.gInsert g name addr  in 
                    (newGlobals, newHeap)

final :: S.TiState -> Bool 
final ([], _, _, _, _) =  error "Empty stack"
final ([node], _, heap, _,_ ) = 
    S.isDataNode (H.hLookup heap node)
final _ = False

glue :: S.TiState -> S.TiState 
glue = id 

--------------------- Steps -----------------------------------

appStep :: S.TiState -> H.Addr -> H.Addr -> S.TiState 
appStep (stack, dump, heap, globals, stat) a1 _ = (a1 : stack, dump, heap, globals, stat)

instanciate :: L.CoreExpr -> S.TiHeap -> S.TiGlobals -> (S.TiHeap, H.Addr)
instanciate (L.ENum n) heap _ = H.hAlloc heap (S.NNum n) 
instanciate (L.EAp f arg) heap globals = H.hAlloc heap2 (S.NApp a1 a2)
    where (heap1, a1) = instanciate f heap globals 
          (heap2, a2) = instanciate arg heap1 globals
instanciate (L.EVar v) heap globals = (heap, S.gLookup globals v)
instanciate (L.ELet isRec binds body) heap globals =
    let (newHeap, newGlobals) = 
            foldl (\ (h, g) (name, expr) -> 
                let (h', addr) = instanciate expr h (if isRec then newGlobals else g)
                    g' = S.gInsert g name addr 
                in (h', g')) 
            (heap, globals) binds 
    in
    instanciate body newHeap newGlobals
instanciate _ _ _= undefined            

combStep :: S.TiState -> L.Name -> [L.Name] -> L.CoreExpr -> S.TiState 
combStep (stack, dump, heap, globals, stat) _ pars body =
    (newAddr : restStack, dump, resultHeap, globals, stat)

    where  
        (instBodyHeap, newAddr) = instanciate body heap newGlobals

        (headAddr, restStack) =
             (head newStack, tail newStack)
             where newStack = drop (length pars) stack

        resultHeap = H.hUpdate instBodyHeap headAddr (S.NInd newAddr) 

        newGlobals  
            | length args >= length pars = S.gUnion globals newMap  
            | otherwise = error "Not enough arguments"
            where 
                args =  getArgs $ tail  stack 
                newMap = S.gFromList $ zip pars args

                getArgs :: S.TiStack -> [] H.Addr 
                getArgs = map getArg
                    where 
                    getArg addr = 
                        case H.hLookup heap addr of 
                        S.NApp _ arg -> arg
                        _ -> error "Not a app in arg lookup"

indStep :: S.TiState -> H.Addr -> S.TiState 
indStep (stack, dump, heap, globals, stat) addr = (addr : tail stack, dump, heap, globals, stat)

step :: S.TiState -> S.TiState 
step state = dispatch (H.hLookup heap (List.head stack))
    where   
        (stack, _, heap, _, _) = state 

        dispatch (S.NNum n) = error $ "Number " ++ show n ++ " applyed as function \n Stack: \n" ++ P.display (showStack stack heap)
        dispatch (S.NApp f a) = appStep state f a
        dispatch (S.NSumpercomb name args body) = combStep state name args body         
        dispatch (S.NInd addr) = indStep state addr

--------------------- Eval -----------------------------------

start :: S.TiState -> [S.TiState]
start state = state : restStates 
    where 
        restStates  
            | final state = [] 
            | otherwise = start $ glue $ step state  

getResult :: [S.TiState] -> Int
getResult ls = n
    where 
        n = 
            case H.hLookup heap (head stack) of 
                S.NNum num -> num 
                _ -> error "Number expected after execution"
        
        (stack, _, heap, _, _) = last ls

run :: L.CoreProgram -> Int
run program = result
    where 
        compiled = compile program 
        states = start compiled 
        result = getResult states

------------------------ Show ---------------------------

showResult :: [S.TiState] -> String
showResult states = P.display $ P.interleav P.nl $ map showState states

showState :: S.TiState -> P.T  
showState (stack, _, heap, _, _) = 
     P.interleav P.nl [
             P.str "======== Stack =========", 
             showStack stack heap,
             P.str "********* Heap **********", 
             S.hShow heap ]

showStack :: S.TiStack -> S.TiHeap -> P.T 
showStack stack heap = 
    P.interleav sep $ map showItem $ reverse stack
    where 
        showItem addr =
            P.merge [
                P.str "addr: ",showAddr addr, P.nl,
                S.showNode (H.hLookup heap addr) heap  
            ]
        sep = P.merge [ P.nl, P.str "------ Frame ------", P.nl ]

showAddr :: H.Addr -> P.T 
showAddr a = P.merge [ P.str "(addr: ", P.str $ show a, P.str ")" ]  

showStat :: S.TiStat -> P.T 
showStat = P.str . show 
            