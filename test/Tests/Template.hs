module Tests.Template (tests) where  

import qualified Frontend as F (parse)
import qualified Template as T

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
            \ main = f 3 4", 4),
            ("main = negate (I 2)", -2),
            ("main = negate (negate (I 2))", 2),
            ("main = twice negate 2", 2),
            ("main = 2 + 2", 4),
            ("main = (I (I 2)) * 4", 8),
            ("main = 6 / 2", 3),
            ("main = 8 - 10", -2),
            ("main = (I 3) - 10", -7),
            ("main = 3 * (I (I 4))", 12),
            ("main = if (1 == 1) 0 1", 0),
            ("main = if (4 < 3) 0 1", 1),
            ("ceil x = if (x > 1) (ceil (x - 1)) x; main = ceil 5", 1),
            ("sum res x = if (x == 0) res (sum (res + x) (x - 1)); main = sum 0 3", 6),
             ("fac n = if (n <> 0) ((fac (n - 1)) * n )  1  ;  main = fac 3", 6) ]

run :: String -> Int
run = T.run . F.parse 

tests :: TestTree
tests = 
    testGroup "Mark1" $
    map (\ (str, expected) -> 
            let 
                actual = run str
            in 
                 testCase str $ actual @?= expected)
        cases 

