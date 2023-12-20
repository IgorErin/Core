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

        (startHeap, startGlobals) = S.buildInitHeap scDefs L.primitives 

eval :: S.TiState -> [S.TiState]
eval state = state : rest 
    where 
        rest 
            | S.final state = []
            | otherwise     = eval $ doAdmin $ step state

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
doAdmin = S.mapStat S.tiStatSucc

---------------- Step -------------------------------

step :: S.TiState -> S.TiState 
step ([], _, _, _, _) = error "empty stack in step"
step state@(hd : _, _, heap, _, _) = dispatch $ H.hLookup heap hd
    where 
    dispatch :: S.Node -> S.TiState
    dispatch (S.NNum n) = numStep n state
    dispatch (S.NApp f x) = appStep f x state
    dispatch (S.NSupercomb name args body) = combStep name args body state
    dispatch (S.NInd addr) = indStep addr state
    dispatch (S.NPrim name prim) = primStep name prim state  
    dispatch (S.NData tag adds) = dataStep tag adds state

dataStep :: Int -> [H.Addr] -> S.TiState -> S.TiState 
dataStep _ _ (stack, [], heap, _, _) = error $ "Data step with emtpy dump.\nStack:\n" ++ Show.strStack stack heap   
dataStep _ _ (_, hd : tl, heap, globs, stat) = (hd, tl, heap, globs, stat) 

primStep :: L.Name  -> L.Primitive -> S.TiState -> S.TiState 
primStep _ L.Neg (stack, dump, heap, globs, stat) =
        case argNode of
         S.NNum n -> 
            let node = S.NNum (- n) 
                heap' = H.hUpdate heap appAddr node  
            in ([appAddr], dump, heap', globs, stat)
         _ -> ([argAddr], [appAddr] : dump, heap, globs, stat)
    where 
    appAddr = case stack of 
            [_, a2'] ->  a2'   
            _ -> error "Neg arg lookup"

    appNode = H.hLookup heap appAddr 

    argAddr = case appNode of 
            S.NApp _ argAddr' -> argAddr'
            _                -> error "Not a app node"

    argNode = H.hLookup heap argAddr
primStep _ L.Add state = primBin (mkArithmOp (+)) state
primStep _ L.Sub state = primBin (mkArithmOp (-)) state
primStep _ L.Div state = primBin (mkArithmOp div) state
primStep _ L.Mul state = primBin (mkArithmOp (*)) state  
primStep _ (L.PrimConstr tag arity) (stack@(_ : tl), dump, heap, globs, stat) =
    (drop arity tl, dump, heap', globs, stat)
    where 
        node = S.NData tag argsAdds 

        heap' = H.hUpdate heap topNodeAddr node

        topNodeAddr = stack !! arity

        argsAdds 
            | length tl < arity = 
                let getArg x = 
                        case H.hLookup heap x of 
                        S.NApp _ argAddr -> argAddr 
                        node'             -> error $ "Unexpected node, Application expected, but got: " ++ show node'
                in take arity $ map getArg tl 
            | otherwise         = error "Arg length mismatch"
primStep _ L.If ([_, condApp, tApp, eApp], dump, heap, globs, stat) =
    case condNode of 
    -- False 
    S.NData 0 [] -> ([elseAddr], dump, heap, globs, stat)
    -- True 
    S.NData 1 [] -> ([thenAddr], dump, heap, globs, stat)
    S.NData _ _ -> error "Tag mismatch in if claus"
    S.NNum _ -> error "Num not in if cond"
    _ -> ([condAddr], [eApp] : dump, heap, globs, stat)
    where 
    getArg x = case H.hLookup heap x of 
                S.NApp _ right -> right 
                _  -> error "App expected in If step"

    condAddr = getArg condApp 
    condNode =  H.hLookup heap condAddr  
            
    thenAddr = getArg tApp 
    elseAddr = getArg eApp
primStep _ L.Greater state = primBin (mkRelOp (>)) state 
primStep _ L.GreaterEq state = primBin (mkRelOp (>=)) state 
primStep _ L.Less state = primBin (mkRelOp (<)) state 
primStep _ L.LessEq state = primBin (mkRelOp (<=)) state 
primStep _ L.Eq state = primBin (mkRelOp (==)) state 
primStep _ L.NotEq state = primBin (mkRelOp (/=)) state 
primStep _ _ ([], _, _, _, _) = error "Empty stack in constructor application"
primStep _ _ _ = undefined

------------------- Bin arithmetic ops ----------------------

primBin :: (Int -> Int -> S.Node) -> S.TiState  -> S.TiState
primBin opp (stack, dump, heap, globs, stat) =
        case(lowArgNode, topArgNode)  of
         (S.NNum left, S.NNum right) -> 
            let node = left `opp` right
                heap' = H.hUpdate heap topAppAddr node  
            in ([topAppAddr], dump, heap', globs, stat)
         (S.NNum _, _) -> ([topArgAddr], [topAppAddr]: dump, heap, globs, stat)
         (_, _) ->  ([lowArgAddr], [topAppAddr] : dump, heap, globs, stat)
    where 
    (lowAppAddr, topAppAddr) = case stack of 
            [_, a2', a3'] ->  (a2', a3')   
            _ -> error "Neg arg lookup"

    topArgAddr = getArgAddr $ H.hLookup heap topAppAddr 
    lowArgAddr = getArgAddr $ H.hLookup heap lowAppAddr 

    getArgAddr node = case node of 
            S.NApp _ argAddr' -> argAddr'
            _                -> error "Not a app node" 

    topArgNode = H.hLookup heap topArgAddr 
    lowArgNode = H.hLookup heap lowArgAddr  

--------------- Rel ops ---------------------------------------

false_, true_ :: S.Node
false_ = S.NData 0 [] 
true_  = S.NData 1 []

mkRelOp :: (Int -> Int -> Bool) -> Int -> Int -> S.Node
mkRelOp rel l r = if l `rel` r then true_ else false_

mkArithmOp :: (Int -> Int -> Int) -> Int -> Int -> S.Node 
mkArithmOp op l r = S.NNum $ l `op` r 

---------------------------------------------------------
    
indStep :: H.Addr -> S.TiState -> S.TiState 
indStep addr (_ : stack, dump, heap, globs, stat) = (addr : stack, dump, heap, globs, stat)
indStep _ _ = error "Impossible. Emtpy stack in Indstep"

numStep :: Int -> S.TiState -> S.TiState         
numStep _ (stack, [], heap, _, _) = 
    error $ "Number in step on top of stack. \nStack:\n" ++ Show.strStack stack heap  
numStep _ ([_], hd : tl, h, g, s) = (hd, tl, h, g, s)
numStep _ ([], _, _, _, _) = error "Empty stack in NumStep"
numStep _ _ = error "inconsistent state in NumStep"

appStep :: H.Addr -> H.Addr -> S.TiState -> S.TiState 
appStep a1 a2 (stack@(hd : _), dump, heap, globs, stat) =
    case argNode of 
    S.NInd addr -> 
        let node = S.NApp a1 addr 
            heap' = H.hUpdate heap hd node 
        in (stack, dump, heap', globs, stat)
    _ -> (a1 : stack, dump, heap, globs, stat)
    where argNode = H.hLookup heap a2
appStep _ _ _ = error "App step, emtpy stack"

combStep :: L.Name -> [L.Name] -> L.CoreExpr -> S.TiState -> S.TiState 
combStep _ params body (stack@(_ : tl), dump, heap, globs, stat) = 
    (oldAddr : drop (length params) tl, dump, heap', globs', stat)
    where 
        globs' = S.gUnion (S.gFromList $ zip params args) globs
        heap' = instantiateAndUpdate body oldAddr heap globs'  
        
        oldAddr = stack !! length params   

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
instantiate (L.EVar name) heap globs = (heap, S.gLookup globs name)  
instantiate (L.EAp left right) heap globs = 
    let (heap', addr1) = instantiate left heap globs
        (heap'', addr2) = instantiate right heap' globs
        node = S.NApp addr1 addr2 
    in H.hAlloc heap'' node
instantiate (L.ELet isRec binds body) heap globs =
    let (heap', globs') = foldl (\ (heapAcc, globsAcc) (name, expr) -> 
            let (heapAcc', addr) = instantiate expr heapAcc (if isRec then globs' else globs)
                globsAcc' = S.gInsert globsAcc name addr
            in (heapAcc', globsAcc')) (heap, globs) binds
    in instantiate body heap' globs'
instantiate (L.EConstr tag arity) heap _ =
    let node = S.NPrim "Pack" $ L.PrimConstr tag arity
    in H.hAlloc heap node
instantiate _ _ _ = error "Instantiation mismatch"

instantiateAndUpdate :: L.CoreExpr -> H.Addr -> S.TiHeap -> S.TiGlobals -> S.TiHeap 
instantiateAndUpdate (L.ENum n) addr heap _ = H.hUpdate heap addr (S.NNum n)
instantiateAndUpdate (L.EVar name) addr heap globs = 
    let nameAddr = S.gLookup globs name 
    in H.hUpdate heap addr (S.NInd nameAddr)
instantiateAndUpdate (L.EAp e1 e2) addr heap globs = 
    let (h1, a1) = instantiate e1 heap globs 
        (h2, a2) = instantiate e2 h1   globs 
    in H.hUpdate h2 addr (S.NApp a1 a2)
instantiateAndUpdate (L.ELet isRec binds body) addr heap globs = 
   let (heap', globs') = foldl (\ (heapAcc, globsAcc) (name, expr) -> 
            let (heapAcc', letAddr) = instantiate expr heapAcc (if isRec then globs' else globs)
                globsAcc' = S.gInsert globsAcc name letAddr
            in (heapAcc', globsAcc')) (heap, globs) binds
    in instantiateAndUpdate body addr heap' globs'   
instantiateAndUpdate (L.EConstr tag arity) addr heap _ = 
    let node = S.NPrim "Pack" $ L.PrimConstr tag arity 
    in H.hUpdate heap addr node 
instantiateAndUpdate _ _ _ _ = undefined

        
        