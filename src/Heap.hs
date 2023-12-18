module Heap where 

import qualified Assoc as A 
import qualified PSeq as P 

----------------- Addr -----------------------

type Addr = Int

showAddr :: Addr -> P.T
showAddr = P.str . show

------------------ Heap ----------------------

type Heap a = (Int, [Int], [(Addr, a)]) 

hInit :: Heap a 
hInit = (0, [1..], [])

hAlloc :: Heap a -> a -> (Heap a, Addr)
hAlloc (size, next : free, cts) n = ((size + 1, free, (next, n) : cts), next) 
hAlloc _ _ = error "Empty infinit list"

hUpdate :: Heap a -> Addr -> a -> Heap a 
hUpdate (size, free, heap) addr value = (size, free, (addr, value) : A.delete addr heap)

hFree :: Heap a -> Addr -> Heap a 
hFree = undefined 

hLookup :: Heap a -> Addr -> a 
hLookup (_, _, heap) key = A.lookupWith key heap (error $ "cant find not" ++ show key ++ "in heap") 

hAddresses :: Heap a -> [Addr]
hAddresses (_, _, heap) = [ addrs | (addrs, _) <- heap ] 

hSize :: Heap a -> Int 
hSize (size, _, _) = size  

hNull :: Addr
hNull = 0 

hIsNull :: Addr -> Bool
hIsNull = (== 0)
