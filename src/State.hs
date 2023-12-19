module State where 

import qualified Assoc as A
import qualified Language as L
import qualified Heap as H
import qualified Utils as U

import Data.Maybe (fromMaybe)

---------------- Stack --------------------

type TiStack = [H.Addr]

---------------- TiDump ---------------------

type TiDump = [TiStack]

initTiDump :: TiDump 
initTiDump = []

------------------- TiGlobals-------------------

type TiGlobals = A.T L.Name H.Addr

gLookup :: TiGlobals -> L.Name -> H.Addr 
gLookup globals name =  fromMaybe (error $ "Globals name: " ++ show name ++ " not found") $ A.lookup name globals

gInit :: TiGlobals
gInit = A.init

gInsert :: TiGlobals -> L.Name -> H.Addr -> TiGlobals 
gInsert = A.insert 

gUnion :: TiGlobals -> TiGlobals -> TiGlobals 
gUnion = A.union

gFromList :: [(L.Name, H.Addr)] -> TiGlobals 
gFromList = A.fromList 

------------------- TiHeap ---------------------

type TiHeap = H.Heap Node 

buildInitHeap :: L.CoreProgram -> [(L.Name, L.Primitive)]->  (TiHeap, TiGlobals) 
buildInitHeap program prims = (heap2, gFromList $ names ++ prims')
    where 
        (heap1, names) = U.mapAccum (\ heap (name, args, body) -> 
            let node = NSupercomb name args body 
                (heap', addr) = H.hAlloc heap node 
            in (heap', (name, addr))) H.hInit program  

        (heap2, prims') = U.mapAccum (\ heap (name, prim) -> 
            let node = NPrim name prim 
                (heap', addr) = H.hAlloc heap node 
            in (heap', (name, addr))) heap1 prims  

-------------------- Node ----------------------

data Node = 
    NApp H.Addr H.Addr 
    | NSupercomb L.Name [L.Name] L.CoreExpr 
    | NNum Int 
    | NInd H.Addr
    | NPrim L.Name L.Primitive
    deriving Show 

isDataNode :: Node -> Bool 
isDataNode (NNum _) = True 
isDataNode _        = False 

------------------- TiStats ------------------

type TiStat = Int

initTiStat :: TiStat
initTiStat = 0

tiStatSucc :: TiStat -> TiStat
tiStatSucc = succ  

tiStatToInt :: TiStat -> Int 
tiStatToInt = id

------------------- State -----------------

type TiState = (TiStack, TiDump, TiHeap, TiGlobals, TiStat)

mapStat :: (TiStat -> TiStat) -> TiState -> TiState
mapStat f (stack, dump, heap, globals, stat) = (stack, dump, heap, globals, f stat)

-- TODO rework with record syntax

final :: TiState -> Bool 
final ([], _, _, _, _) =  error "emtpy stack in final" 
final ([hd], [], heap, _, _) = isDataNode $ H.hLookup heap hd
final (_, _, _,_, _) = False 