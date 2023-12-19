module Template where 

import qualified Language as L 
import qualified Heap as H 
import qualified State as S 
import qualified Show

compile :: L.CoreProgram -> S.TiState 
compile program = 
    (startStack, S.initTiDump, startHeap, startGlobals, S.initTiStat)
    where 
        scDefs = L.stdlib ++ program 
        startStack = [S.gLookup startGlobals "main"] 

        (startHeap, startGlobals) = S.buildInitHeap scDefs 

eval :: S.TiState -> [S.TiState]
eval state = state : rest 
    where 
        rest 
            | S.final state = []
            | otherwise     = eval $ step state

getResult :: [S.TiState] -> Int
getResult states = get $ H.hLookup heap $ head stack 
    where
        (stack, _, heap, _, _) = last states 

        get (S.NNum n) = n 
        get _ = error "Result expected as num"

run :: L.CoreProgram -> Int
run = getResult . eval . compile 
                    
--------------- Admin ------------------------

doAdmin :: S.TiState -> S.TiState 
doAdmin = id

---------------- Step -------------------------------

step :: S.TiState -> S.TiState 
step ([], _, _, _, _) = error "empty stack in step"
step state@(hd : _, _, heap, _, _) = dispatch $ H.hLookup heap hd
    where 
        dispatch :: S.Node -> S.TiState
        dispatch (S.NNum n) = numStep n state
        dispatch (S.NApp f x) = appStep f x state
        dispatch (S.NSupercomb name args body) = combStep name args body state  

numStep :: Int -> S.TiState -> S.TiState         
numStep _ (stack, _, heap, _, _) = 
    error $ "Number in step on top of stack. \nStack:\n" ++ Show.strStack stack heap  

appStep :: H.Addr -> H.Addr -> S.TiState -> S.TiState 
appStep a1 _ (stack, dump, heap, globs, stat) =
    (a1: stack, dump, heap, globs, stat)

combStep :: L.Name -> [L.Name] -> L.CoreExpr -> S.TiState -> S.TiState 
combStep _ params body (_ : tl, dump, heap, globs, stat) = 
    (addr : drop (length params) tl, dump, heap', globs', stat)
    where 
        globs' = S.gUnion (S.gFromList $ zip params args) globs

        (heap', addr) = instantiate body heap globs'  

        args 
            | length params <= length tl  = 
                let getAddr (S.NApp _ a) = a 
                    getAddr node = error $ "Not an App in arg lookup: " ++ show node
                in
                map (getAddr . H.hLookup heap) tl
            | otherwise = error "Arg length mismatch"
combStep _ _ _ _= error "Empty stack on comb step"

instantiate :: L.CoreExpr -> S.TiHeap -> S.TiGlobals -> (S.TiHeap, H.Addr)
instantiate (L.ENum n) heap _ = H.hAlloc heap $ S.NNum n  
instantiate (L.EVar name) heap globs =
    (heap, S.gLookup globs name)  
instantiate (L.EAp left right) heap globs = 
    let (heap', addr1) = instantiate left heap globs
        (heap'', addr2) = instantiate right heap' globs
        node = S.NApp addr1 addr2 
    in H.hAlloc heap'' node
instantiate _ _ _ = error "Instantiate mismatch"

        
        