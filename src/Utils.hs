module Utils where 

import qualified Language as L 
import qualified PSeq as P

import qualified Data.Maybe as MB 
import qualified Data.Map.Lazy as M (Map, empty, lookup, keys, size, insert, delete, toList)

------------- Adr -------------------------
type Addr = Int 

diffAddr :: Int
diffAddr = - 1 

addrIsNull :: TiHeap -> Bool 
addrIsNull = null

showAddr :: Addr -> TiHeap  -> P.T 
showAddr addr heap = showNode (hLook addr heap) heap  

type TiStack = [Addr]

--------------- Node -------------------------
data Node = 
    NApp Addr Addr 
    | NCombinator L.Name [L.Name] L.CoreExpr 
    | NNum Int 
    deriving Show 

isDataNode :: Node -> Bool 
isDataNode (NNum _) = True 
isDataNode _ = False 

showNode :: Node -> TiHeap -> P.T
showNode (NNum n) _ =  P.merge [P.str "NNum ", P.str $ show n]
showNode (NApp f x) heap = P.merge [ P.str "NApp ", showNode nf heap , P.str " (", showNode nx heap , P.str ")"  ]
    where 
        nf = hLook f heap 
        nx = hLook x heap 

showNode (NCombinator name _ _ ) _ = P.merge [P.str name]


---------------- Dump -------------------------
data TiDump = DummyDump deriving Show 

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

hShow :: TiHeap -> P.T 
hShow heap = P.interleav P.nl . map (\(key, value) -> P.merge [P.str $ show key, P.str " -> ", showNode  value heap ]) . M.toList . fst $ heap 

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