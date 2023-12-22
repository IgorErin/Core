{-# LANGUAGE TemplateHaskell #-}

module GMachine where 

import Control.Lens 
import qualified Heap as H
import qualified Assoc as A

runProg :: String -> String 
runProg = undefined

---------------- GmCode -------------------

type Name = String

data Instruction = 
    Unwind 
    | Pushglobal Name
    | Pushint Int 
    | Push Int 
    | Mkap 
    | Slide Int 
    deriving (Eq, Show)

type GmCode = [Instruction]

--------------- GmStack ----------------

type Addr = Int 
type GmStack = [Addr]

--------------- Node ------------------

data Node = 
    NNum Int 
    | NAp Addr Addr 
    | NGlobal Int GmCode

---------------- GMHeap ----------------

type GmHeap = H.Heap Node 

----------------- GmStat ----------------

newtype GmStat = GmStat { _stepCount :: Int }

makeLenses ''GmStat 

------------------ GmGlobals ----------

type GmGlobals = A.T Name Addr

---------------- GmState --------------

data GmState = GmState {
    _code :: GmCode, 
    _stack :: GmStack, 
    _heap :: GmHeap,
    _globs :: GmGlobals,
    _stat :: GmStat
}

makeLenses ''GmState

stackTop :: GmState -> Addr 
stackTop state = case view stack state of 
                    hd : _ -> hd 
                    _      -> error "Stack empty. Not top element."

pop :: GmState -> (GmState, Addr)
pop state = (over stack (drop 1) state, head $ view stack state)

stackItem :: GmState -> Int -> Addr 
stackItem state n = view stack state !! n   

----------------- Eval -------------------

eval :: GmState -> [GmState]
eval state = state : restStates
    where 
    restStates | final state = []
               | otherwise   = eval nextState 

    nextState = doAdming $ step state

final :: GmState -> Bool 
final = undefined

doAdming :: GmState -> GmState 
doAdming = over (stat . stepCount) (+1) 

step :: GmState -> GmState
step state = dispatch headInst (set code tlInsts state)
    where 
    (headInst, tlInsts) = case  view code state of 
             hd : tl -> (hd, tl)
             _       -> error "Emtpy code"

gmFinale :: GmState -> Bool
gmFinale = null . view code

------------------ Dispatch ------------------------

dispatch :: Instruction -> GmState -> GmState 
dispatch (Pushglobal f) = pushGlobal f 
dispatch (Pushint n)    = pushInt n 
dispatch Mkap           = mkap 
dispatch (Push n)       = push n 
dispatch (Slide n)      = slide n
dispatch Unwind         = unwind

pushGlobal :: Name -> GmState -> GmState 
pushGlobal name state = 
    let addr = A.lookupWith name (view globs state) (error $ "Global addr not found for " ++ name)     
    in over stack (addr:) state 

pushInt :: Int -> GmState -> GmState 
pushInt number state = 
    let (heap', addr) = H.hAlloc (view heap state) (NNum number) 
    in over stack (addr:) . set heap heap' $ state  

mkap :: GmState -> GmState 
mkap state = 
    let (state', a1) = pop state 
        (state'', a2) = pop state' 

        node = NAp a1 a2 
        h = view heap state'' 
        (h', addr) = H.hAlloc h node 
    in over stack (addr:) $ set heap h' state'' 

push :: Int -> GmState -> GmState 
push number state = 
    let addr = stackItem state $ number + 1 
        topAddr = 
            case H.hLookup (view heap state) addr of 
                NAp _ right -> right 
                _           -> error "In push expected NAp node"
                
    in over stack (topAddr:) state 

unwind :: GmState -> GmState 
unwind state = undefined
    where 
        




    
