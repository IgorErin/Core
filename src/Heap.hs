module Heap where 

import qualified Assoc as A 
import qualified PSeq as P 

----------------- Addr -----------------------

type Addr = Int

showAddr :: Addr -> P.T
showAddr = P.str . show
------------------ Info ---------------------

data Info = Info { heapAllocCount :: Int, heapSize :: Int, heapReallocCount :: Int }

initInfo :: Info
initInfo = Info { heapAllocCount = 0, heapSize = 0, heapReallocCount = 0}

------------------ Heap ----------------------

type Heap a = (Info, [Int], [(Addr, a)]) 

hInit :: Heap a 
hInit = (initInfo, [1..], [])

hAlloc :: Heap a -> a -> (Heap a, Addr)
hAlloc (info, next : free, cts) n = 
    let newInfo = info { heapAllocCount = heapAllocCount info + 1,
                         heapSize       = heapSize info + 1 }
    in
    ((newInfo, free, (next, n) : cts), next) 
hAlloc _ _ = error "Empty infinit list"

hUpdate :: Heap a -> Addr -> a -> Heap a 
hUpdate (info, free, heap) addr value = 
    let newInfo = info { heapReallocCount = heapReallocCount info + 1 } 
    in (newInfo, free, (addr, value) : A.delete addr heap)

hFree :: Heap a -> Addr -> Heap a 
hFree = undefined 

hLookup :: Heap a -> Addr -> a 
hLookup (_, _, heap) key = A.lookupWith key heap (error $ "cant find not" ++ show key ++ "in heap") 

hAddresses :: Heap a -> [Addr]
hAddresses (_, _, heap) = [ addrs | (addrs, _) <- heap ] 

hSize :: Heap a -> Info 
hSize (info, _, _) = info  

hNull :: Addr
hNull = 0 

hIsNull :: Addr -> Bool
hIsNull = (== 0)
