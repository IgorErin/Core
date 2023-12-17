module Tests.Mark1 (tests) where  

import qualified Frontend as F (parse)
import qualified Mark1 as M

import Test.Tasty
import Test.Tasty.HUnit

cases :: [(String, Int)]
cases = 
    [ ("main = I 3", 3),
      ("id = S K K ; \
        \ main = id 3", 3),
        ("id = S K K ; \
         \ main = twice twice twice id 3", 3),
      ("main = S K K 3", 3),
      ("main = (S K I K) 3 0", 3),
      ("main = (S K I (K I S)) 0", 0),
      ("main = let x = 1 in x", 1),
      ("main = letrec x = K y 1; y = K 2 x in x", 2) ,
      ("main = twice (I I I) 3", 3),
       (" cons a b cc cn = cc a b ; \
        \ nil      cc cn = cn ; \
        \ hd list = list K abort ; \
        \ tl list = list K1 abort ; \
        \ abort = abort ; \ 
        \ infinite x = cons x (infinite x) ; \
        \ main = hd (tl (infinite 4))", 4),
        (" main = let id1 = I I I\
          \ in id1 id1 3", 3),
        ("oct g x = \ 
         \   let h = twice g \
         \    in  \
         \    let k = twice h \ 
         \     in k (k x) ; \
         \ main = oct I 4", 4),
         ("cons a b cc cn = cc a b ; \
          \ nil      cc cn = cn ; \
          \ hd list = list K abort ; \
          \ tl list = list K1 abort ; \
          \ abort = abort ; \ 
          \ infinite x = \ 
          \  letrec xs = cons x xs \
          \  in xs ; \
          \  main = hd (tl (tl (infinite 4)))", 4),
          ("  pair x y f = f x y ; \
            \ fst p = p K ; \
            \ snd p = p K1 ; \
            \ f x y = letrec \
            \ a = pair x b ; \
            \ b = pair y a \
            \ in \
            \ fst (snd (snd (snd a))) ; \
            \ main = f 3 4", 4)]

run :: String -> Int
run = M.run . F.parse 

tests :: TestTree
tests = 
    testGroup "Mark1" $
    map (\ (str, expected) -> 
            let 
                actual = run str
            in 
                 testCase str $ actual @?= expected)
        cases 

