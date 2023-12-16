module Utils where 

import qualified Data.Map as M (Map, empty, lookup, keys, size, insert, delete)
import qualified Language as L 
import qualified Data.Maybe as MB 

------------- Adr -------------------------
type Addr = Int 

diffAddr :: Int
diffAddr = - 1 

addrIsNull :: TiHeap -> Bool 
addrIsNull = null

type TiStack = [Addr]

--------------- Node -------------------------
data Node = 
    NApp Addr Addr 
    | NCombinator L.Name [L.Name] L.CoreExpr 
    | NNum Int 

isDataNode :: Node -> Bool 
isDataNode (NNum _) = True 
isDataNode _ = False 
---------------- Dump -------------------------
data TiDump = DummyDump

initDump :: TiDump
initDump = DummyDump

------------------- TiHeap --------------------
type TiHeap = (M.Map Addr Node, [Int])

hInit :: TiHeap 
hInit = (M.empty, iterate (+1) 0)

hLook :: Addr -> TiHeap -> Node 
hLook addr (h, _) = MB.fromMaybe (error $ "hLook faild on " ++ show addr) $ M.lookup addr h   

hAddrs :: TiHeap -> [Addr]
hAddrs (h, _)= M.keys h 

hSize :: TiHeap -> Int 
hSize (h, _)= M.size h

hAlloc :: Node -> TiHeap -> (TiHeap, Addr)
hAlloc value (h, current: next) = 
    let newHeap = (M.insert current value h, next) in
    (newHeap, current)
hAlloc _ _ = error "empty list in hAlloc"

hFree :: TiHeap -> Addr -> TiHeap 
hFree (heap, addrs) a = (M.delete a heap, a: addrs)

-------------- TiGlobals -----------------------
type TiGlobals = M.Map L.Name Addr

gInit :: TiGlobals 
gInit = M.empty

gLook :: L.Name -> TiGlobals -> Addr 
gLook k m = MB.fromMaybe (error $ "gLook faild on " ++ show k) $ M.lookup k m   

gInsert :: L.Name -> Addr -> TiGlobals -> TiGlobals
gInsert = M.insert  

----------------- TiStat -----------------------
type TiStat = Int 

tiStatInit :: TiStat 
tiStatInit = 0 
tiStatInc :: TiStat -> TiStat 
tiStatInc = succ 
tiStatGet :: TiStat -> Int 
tiStatGet x = x
------------------- TiState ----------------------

type TiState = (TiStack, TiDump, TiHeap, TiGlobals, TiStat)

mapStat :: (TiStat -> TiStat) -> TiState -> TiState
mapStat f (s, d, h, g, stat) = (s, d, h, g, f stat)