module State where 

import qualified Assoc as A
import qualified Language as L
import qualified PSeq as P
import qualified Heap as H


import Data.Maybe (fromMaybe)

---------------- Stack --------------------

type TiStack = [H.Addr]

---------------- TiDump ---------------------

type TiDump = ()

initTiDump :: TiDump 
initTiDump = ()

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

hShow :: TiHeap -> P.T 
hShow heap = 
    P.interleav P.nl . 
    map (\(key, value) -> P.merge [P.str $ show key, P.str " -> ", showNode  value heap ]) .
    (\(_, _, h) -> h) $
    heap 

-------------------- Node ----------------------

data Node = 
    NApp H.Addr H.Addr 
    | NSumpercomb L.Name [L.Name] L.CoreExpr 
    | NNum Int 
    | NInd H.Addr

isDataNode :: Node -> Bool 
isDataNode (NNum _) = True 
isDataNode _        = False 

showNode :: Node -> TiHeap -> P.T
showNode (NNum n) _ =  P.merge [P.str "NNum ", P.str $ show n]
showNode (NApp f x) heap = P.merge [ P.str "NApp ", showNode nf heap , P.str " (", showNode nx heap , P.str ")"  ]
    where 
        nf = H.hLookup heap f 
        nx = H.hLookup heap x

showNode (NSumpercomb name _ _ ) _ = P.merge [P.str name]
showNode (NInd addr) heap = P.merge [P.str "NInd ", showNode (H.hLookup heap addr) heap ]

------------------- TiStats ------------------

type TiStat = Int

tiStatInit :: TiStat
tiStatInit = 0

tiStatSucc :: TiStat -> TiStat
tiStatSucc = succ  

tiStatToInt :: TiStat -> Int 
tiStatToInt = id

------------------- State -----------------

type TiState = (TiStack, TiDump, TiHeap, TiGlobals, TiStat)

mapStat :: (TiStat -> TiStat) -> TiState -> TiState
mapStat f (stack, dump, heap, globals, stat) = (stack, dump, heap, globals, f stat)
