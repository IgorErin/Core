module Utils where 

import qualified Language as L 
import qualified PSeq as P
import qualified Assoc as A

import qualified Data.Maybe as MB 

------------- Addr -------------------------
type Addr = Int 

diffAddr :: Int
diffAddr = - 1 

addrIsNull :: TiHeap -> Bool 
addrIsNull (_, ls, _) = A.null ls   

showAddr :: Addr -> TiHeap  -> P.T 
showAddr addr heap = showNode (hLook addr heap) heap  

type TiStack = [Addr]

--------------- Node -------------------------
data Node = 
    NApp Addr Addr 
    | NCombinator L.Name [L.Name] L.CoreExpr 
    | NNum Int 
    | NInd Addr
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
showNode (NInd addr) heap = showNode (hLook addr heap) heap

---------------- Dump -------------------------
data TiDump = DummyDump deriving Show 

initDump :: TiDump
initDump = DummyDump

------------------- TiHeap --------------------
type TiHeap = (Int, A.T Addr Node, [Addr])

hInit :: TiHeap 
hInit = (0, [], iterate (+1) 0)

hLook :: Addr -> TiHeap -> Node 
hLook addr (_, h, _) = MB.fromMaybe (error $ "hLook faild on " ++ show addr) $ A.look addr h   

hAddrs :: TiHeap -> [Addr]
hAddrs (_ ,h, _)= map fst h 

hSize :: TiHeap -> Int 
hSize (size, _, _)= size

hAlloc :: Node -> TiHeap -> (TiHeap, Addr)
hAlloc value (size, h, newAddr : rest) =  ((size + 1, (newAddr, value) : h, rest), newAddr)
hAlloc _ _ = error "empty addr list in hAlloc"

hFree :: TiHeap -> Addr -> TiHeap 
hFree (size, heap, addrs) a = (size - 1, A.delete a heap, a: addrs)

hShow :: TiHeap -> P.T 
hShow heap = 
    P.interleav P.nl . 
    map (\(key, value) -> P.merge [P.str $ show key, P.str " -> ", showNode  value heap ]) .
    (\(_, h, _) -> h) $
    heap


hUpdate :: Addr -> Node -> TiHeap -> TiHeap 
hUpdate key value (size, ls, adds) = (size, A.update key value ls, adds)

-------------- TiGlobals -----------------------
type TiGlobals = A.T L.Name Addr

gInit :: TiGlobals 
gInit = []

gLook :: L.Name -> TiGlobals -> Addr 
gLook k m = MB.fromMaybe (error $ "gLook faild on " ++ show k) $ A.look k m   

gInsert :: L.Name -> Addr -> TiGlobals -> TiGlobals
gInsert name addr gl = (name, addr) : gl    

gUnion :: TiGlobals -> TiGlobals ->  TiGlobals 
gUnion = A.union

gFromList :: [(L.Name, Addr)] -> TiGlobals 
gFromList = A.fromList 

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